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

struct cmd * TSkip() {
  struct cmd * res = new_cmd_ptr();
  res -> t = T_SKIP;
  res -> d.SKIP.__ = NULL;
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

static char * prime_string(const char * s) {
  if (s == NULL) return NULL;
  size_t n = strlen(s);
  char * r = (char *) malloc(n + 2);
  if (r == NULL) {
    printf("Failure in malloc.\n");
    exit(0);
  }
  memcpy(r, s, n);
  r[n] = '\'';
  r[n + 1] = '\0';
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
        char* quote = prime_string(x);
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
        char * quote = prime_string(P->d.QUANT.name);
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

static int eq_str(const char *a, const char *b) {
  if (a == NULL && b == NULL) return 1;
  if (a == NULL || b == NULL) return 0;
  return strcmp(a, b) == 0;
}

int EqualExprInt(struct expr_int *a, struct expr_int *b) {
  if (a == NULL && b == NULL) return 1;
  if (a == NULL || b == NULL) return 0;
  if (a->t != b->t) return 0;
  switch (a->t) {
    case T_CONST:
      return a->d.CONST.value == b->d.CONST.value;
    case T_VAR:
      return eq_str(a->d.VAR.name, b->d.VAR.name);
    case T_BINOP:
      if (a->d.BINOP.op != b->d.BINOP.op) return 0;
      if (!EqualExprInt(a->d.BINOP.left, b->d.BINOP.left)) return 0;
      if (!EqualExprInt(a->d.BINOP.right, b->d.BINOP.right)) return 0;
      return 1;
  }
  return 0;
}

int EqualExprBool(struct expr_bool *a, struct expr_bool *b) {
  if (a == NULL && b == NULL) return 1;
  if (a == NULL || b == NULL) return 0;
  if (a->t != b->t) return 0;
  switch (a->t) {
    case T_TRUE:
      return 1;
    case T_FALSE:
      return 1;
    case T_CMP:
      if (a->d.CMP.op != b->d.CMP.op) return 0;
      if (!EqualExprInt(a->d.CMP.left, b->d.CMP.left)) return 0;
      if (!EqualExprInt(a->d.CMP.right, b->d.CMP.right)) return 0;
      return 1;
    case T_PROP_BINOP:
      if (a->d.PROP_BINOP.op != b->d.PROP_BINOP.op) return 0;
      if (!EqualExprBool(a->d.PROP_BINOP.left, b->d.PROP_BINOP.left)) return 0;
      if (!EqualExprBool(a->d.PROP_BINOP.right, b->d.PROP_BINOP.right)) return 0;
      return 1;
    case T_PROP_UNOP:
      if (a->d.PROP_UNOP.op != b->d.PROP_UNOP.op) return 0;
      if (!EqualExprBool(a->d.PROP_UNOP.arg, b->d.PROP_UNOP.arg)) return 0;
      return 1;
    case T_QUANT:
      if (a->d.QUANT.op != b->d.QUANT.op) return 0;
      if (!eq_str(a->d.QUANT.name, b->d.QUANT.name)) return 0;
      if (!EqualExprBool(a->d.QUANT.arg, b->d.QUANT.arg)) return 0;
      return 1;
  }
  return 0;
}

static int tests_passed = 0;
static int tests_failed = 0;

static void record_result(int cond, const char *name) {
  if (cond) {
    tests_passed++;
    printf("PASS: %s\n", name);
  } else {
    tests_failed++;
    printf("FAIL: %s\n", name);
  }
}

static void run_tests() {
  /* 构造命令 skip
     验证：TSkip() 生成的命令 s 满足 s != NULL 且 s->t == T_SKIP，
     即返回的命令类型枚举值与期望相等（用于后续 P2Q skip 测试）。 */
  {
    struct cmd * s = TSkip();
    record_result(s != NULL && s->t == T_SKIP, "TSkip returns T_SKIP");
  }

  /* 克隆算术表达式
     验证：CloneExprInt(e) 产生的 c 与 e 结构相等，
     即 EqualExprInt(e, c) == 1；包括操作符、左右子树和值均逐层相等。 */
  {
    struct expr_int * e = TBinOp(T_MUL,
      TBinOp(T_PLUS, TVar("x"), TConst(1)),
      TConst(2)
    );
    struct expr_int * c = CloneExprInt(e);
    record_result(EqualExprInt(e, c), "CloneExprInt deep equality");
  }

  /* 克隆布尔表达式（含量词/逻辑联结）
     验证：CloneExprBool(b) 产生的 c 与 b 结构相等，
     即 EqualExprBool(b, c) == 1；覆盖量词、命题一元/二元联结等深度结构。 */
  {
    struct expr_bool * b = TPropBinOp(T_AND,
      TQuant(T_FORALL, "x", TCmp(T_EQ, TVar("x"), TConst(1))),
      TPropUnOp(T_NOT, TPropBinOp(T_IFF, TTrue(), TFalse()))
    );
    struct expr_bool * c = CloneExprBool(b);
    record_result(EqualExprBool(b, c), "CloneExprBool complex equality");
  }

  /* 变量替换（算术，命中）
     验证：SubstInt(TVar("x"), "x") 的结果 r 为 TVar("x'")，
     即 r->t == T_VAR 且 r->d.VAR.name 与期望字符串 "x'" 相等。 */
  {
    struct expr_int * e = TVar("x");
    struct expr_int * r = SubstInt(e, "x");
    record_result(r != NULL && r->t == T_VAR && eq_str(r->d.VAR.name, "x'"), "SubstInt primes matched var");
  }

  /* 变量替换（算术，未命中）
     验证：SubstInt(TVar("y"), "x") 的结果 r 保持不变为 TVar("y")，
     即 r->t == T_VAR 且 r->d.VAR.name 与 "y" 相等。 */
  {
    struct expr_int * e = TVar("y");
    struct expr_int * r = SubstInt(e, "x");
    record_result(r != NULL && r->t == T_VAR && eq_str(r->d.VAR.name, "y"), "SubstInt leaves unmatched var");
  }

  /* 变量替换（布尔量词，命中）
     验证：SubstBool(forall x. (x==1), "x") 的结果 r 同时重命名：
     - 量词绑定名 r->d.QUANT.name == "x'"
     - 体内同名变量也为 "x'"，即 r->d.QUANT.arg->d.CMP.left 为 TVar("x'")。 */
  {
    struct expr_bool * b = TQuant(T_FORALL, "x", TCmp(T_EQ, TVar("x"), TConst(1)));
    struct expr_bool * r = SubstBool(b, "x");
    int ok = r != NULL && r->t == T_QUANT && eq_str(r->d.QUANT.name, "x'") && r->d.QUANT.arg->t == T_CMP && r->d.QUANT.arg->d.CMP.left->t == T_VAR && eq_str(r->d.QUANT.arg->d.CMP.left->d.VAR.name, "x'");
    record_result(ok, "SubstBool primes quant binder and body");
  }

  /* P2Q（赋值）
     验证：令 c = (x := x+1), P = (x > 0)，Q = P2Q(c, P, vcs)
     构造期望 expected = ((SubstInt(x+1, "x") == x) && SubstBool(P, "x"))，
     断言 EqualExprBool(Q, expected) == 1，
     即 Q 与期望弱前置的合取结构相等。 */
  {
    struct expr_bool * P = TCmp(T_GT, TVar("x"), TConst(0));
    struct cmd * c = TAsgn("x", TBinOp(T_PLUS, TVar("x"), TConst(1)));
    struct vc_list * vcs = new_vc_list();
    struct expr_bool * Q = P2Q(c, P, vcs);
    struct expr_bool * expected = TPropBinOp(T_AND,
      TCmp(T_EQ, SubstInt(CloneExprInt(TBinOp(T_PLUS, TVar("x"), TConst(1))), "x"), TVar("x")),
      SubstBool(P, "x")
    );
    record_result(EqualExprBool(Q, expected), "P2Q assignment shape");
  }

  /* P2Q（skip）
     验证：令 c = skip, Q = P2Q(c, P, vcs)，则 Q 与 P 结构相等，
     即 EqualExprBool(Q, P) == 1（skip 不改变前置）。 */
  {
    struct expr_bool * P = TTrue();
    struct cmd * c = TSkip();
    struct vc_list * vcs = new_vc_list();
    struct expr_bool * Q = P2Q(c, P, vcs);
    record_result(EqualExprBool(Q, P), "P2Q skip clones P");
  }

  /* P2Q（顺序）
     验证：令 c = (skip; x := 1)，Q = P2Q(c, P, vcs)
     右侧期望前置 rightPre = P2Q(skip, P, vcs)，
     期望 expected = P2Q(x := 1, rightPre, vcs)，
     断言 EqualExprBool(Q, expected) == 1，说明顺序组合传递正确。 */
  {
    struct expr_bool * P = TTrue();
    struct cmd * c = TSeq(TSkip(), TAsgn("x", TConst(1)));
    struct vc_list * vcs = new_vc_list();
    struct expr_bool * Q = P2Q(c, P, vcs);
    struct expr_bool * rightPre = P2Q(TSkip(), P, vcs);
    struct expr_bool * expected = P2Q(TAsgn("x", TConst(1)), rightPre, vcs);
    record_result(EqualExprBool(Q, expected), "P2Q seq composition");
  }

  /* P2Q（条件）
     验证：令 c = if cond then left else right，Q = P2Q(c, P, vcs)
     构造 thenPre = (P && cond)，elsePre = (P && !cond)
     期望 expected = (P2Q(left, thenPre, vcs) || P2Q(right, elsePre, vcs))
     断言 EqualExprBool(Q, expected) == 1，说明分支组合正确。 */
  {
    struct expr_bool * P = TTrue();
    struct expr_bool * cond = TCmp(T_LT, TVar("x"), TConst(0));
    struct cmd * left = TAsgn("y", TConst(1));
    struct cmd * right = TAsgn("y", TConst(2));
    struct cmd * c = TIf(cond, left, right);
    struct vc_list * vcs = new_vc_list();
    struct expr_bool * Q = P2Q(c, P, vcs);
    struct expr_bool * thenPre = TPropBinOp(T_AND, CloneExprBool(P), CloneExprBool(cond));
    struct expr_bool * elsePre = TPropBinOp(T_AND, CloneExprBool(P), TPropUnOp(T_NOT, CloneExprBool(cond)));
    struct expr_bool * expected = TPropBinOp(T_OR,
      P2Q(left, thenPre, vcs),
      P2Q(right, elsePre, vcs)
    );
    record_result(EqualExprBool(Q, expected), "P2Q if branches");
  }

  /* P2Q（循环）
     验证：令 c = while inv, cond do body，Q = P2Q(c, P, vcs)
     - vcs->size == 2：追加两条 VC（初始化：P -> inv；保持：bodyPost -> inv）
     - 退出前置 EqualExprBool(Q, inv && !cond) == 1：退出条件正确。 */
  {
    struct expr_bool * inv = TCmp(T_LE, TVar("i"), TVar("n"));
    struct expr_bool * cond = TCmp(T_LT, TVar("i"), TVar("n"));
    struct cmd * body = TAsgn("i", TBinOp(T_PLUS, TVar("i"), TConst(1)));
    struct cmd * c = TWhile(inv, cond, body);
    struct expr_bool * P = TTrue();
    struct vc_list * vcs = new_vc_list();
    struct expr_bool * Q = P2Q(c, P, vcs);
    int ok = vcs->size == 2 && EqualExprBool(Q, TPropBinOp(T_AND, CloneExprBool(inv), TPropUnOp(T_NOT, CloneExprBool(cond))));
    record_result(ok, "P2Q while adds two VCs and exit condition");
  }

  /* GenerateVCs（最终蕴含）
     验证：令 p = {require: P, ensure: E, c}，vcs = GenerateVCs(&p)
     取最后一条 VC（cur->f），构造 totalQ = P2Q(c, P, t_vcs)
     期望 expected = (totalQ -> E)
     断言 EqualExprBool(cur->f, expected) == 1，说明最终 VC 正确。 */
  {
    struct cmd * c = TAsgn("x", TConst(1));
    struct expr_bool * req = TTrue();
    struct expr_bool * ens = TCmp(T_EQ, TVar("x"), TConst(1));
    struct full_annotated_cmd p;
    p.require = req;
    p.ensure = ens;
    p.c = *c;
    struct vc_list * vcs = GenerateVCs(&p);
    struct vc_node * cur = vcs->head;
    while (cur && cur->next) cur = cur->next;
    struct vc_list * t_vcs = new_vc_list();
    struct expr_bool * totalQ = P2Q(&(p.c), CloneExprBool(p.require), t_vcs);
    struct expr_bool * expected = TPropBinOp(T_IMPLY, totalQ, CloneExprBool(p.ensure));
    record_result(cur && EqualExprBool(cur->f, expected), "GenerateVCs final implication");
  }

  /* 打印冒烟
     验证：调用 PrintExprBool/PrintExprInt/PrintCmd 可正常输出并覆盖基本分支，
     不作结构比较，仅确认执行稳定无崩溃。 */
  {
    PrintExprBool(TPropBinOp(T_IFF, TTrue(), TFalse()));
    printf("\n");
    PrintExprInt(TBinOp(T_MINUS, TConst(3), TConst(2)));
    printf("\n");
    PrintCmd(TSeq(TAsgn("x", TConst(1)), TSkip()));
    printf("\n");
    record_result(1, "Print functions smoke");
  }

  printf("Tests passed: %d, failed: %d\n", tests_passed, tests_failed);
}

static char * read_all(const char * path) {
  FILE * f = fopen(path, "rb");
  if (!f) return NULL;
  fseek(f, 0, SEEK_END);
  long n = ftell(f);
  fseek(f, 0, SEEK_SET);
  if (n < 0) { fclose(f); return NULL; }
  char * buf = (char *) malloc((size_t)n + 1);
  if (!buf) { fclose(f); return NULL; }
  size_t r = fread(buf, 1, (size_t)n, f);
  fclose(f);
  buf[r] = '\0';
  return buf;
}

struct parse {
  const char * s;
  size_t i;
};

static void p_skip(struct parse *p) {
  for (;;) {
    while (p->s[p->i] == ' ' || p->s[p->i] == '\n' || p->s[p->i] == '\r' || p->s[p->i] == '\t') p->i++;
    unsigned char c0 = (unsigned char)p->s[p->i];
    unsigned char c1 = (unsigned char)p->s[p->i + 1];
    unsigned char c2 = (unsigned char)p->s[p->i + 2];
    if (c0 == 0xEF && c1 == 0xBB && c2 == 0xBF) { p->i += 3; continue; } /* UTF-8 BOM */
    if (c0 == 0xC2 && c1 == 0xA0) { p->i += 2; continue; } /* NBSP */
    if (c0 == 0xE2 && c1 == 0x80 && (c2 == 0x8B || c2 == 0x8C || c2 == 0x8D)) { p->i += 3; continue; } /* zero-width */
    break;
  }
}

static int p_expect(struct parse *p, char c) {
  p_skip(p);
  if (p->s[p->i] == c) { p->i++; return 1; }
  return 0;
}

static int p_match_kw(struct parse *p, const char *kw) {
  p_skip(p);
  size_t j = 0;
  size_t start = p->i;
  while (kw[j] != '\0') {
    if (p->s[p->i] != kw[j]) { p->i = start; return 0; }
    p->i++; j++;
  }
  return 1;
}

static int is_ident_start(char c) { return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_'; }
static int is_ident_char(char c) { return is_ident_start(c) || (c >= '0' && c <= '9'); }

static char * p_ident(struct parse *p) {
  p_skip(p);
  if (!is_ident_start(p->s[p->i])) return NULL;
  size_t start = p->i;
  p->i++;
  while (is_ident_char(p->s[p->i])) p->i++;
  size_t n = p->i - start;
  char * r = (char *) malloc(n + 1);
  memcpy(r, p->s + start, n);
  r[n] = '\0';
  return r;
}

static char * p_ident_anywhere(struct parse *p) {
  for (;;) {
    p_skip(p);
    if (!p->s[p->i]) return NULL;
    if (is_ident_start(p->s[p->i])) return p_ident(p);
    /* skip one byte of unknown/invisible */
    p->i++;
  }
}

static unsigned int p_uint(struct parse *p) {
  p_skip(p);
  unsigned int v = 0;
  int has = 0;
  while (p->s[p->i] >= '0' && p->s[p->i] <= '9') { has = 1; v = v * 10 + (unsigned int)(p->s[p->i] - '0'); p->i++; }
  if (!has) return 0;
  return v;
}

static struct expr_int * parse_int_expr(struct parse *p);
static struct expr_bool * parse_bool_expr(struct parse *p);
static struct cmd * parse_cmd(struct parse *p);

static struct expr_int * parse_int_expr(struct parse *p) {
  p_skip(p);
  if (p_match_kw(p, "const")) { if (!p_expect(p, '(')) return NULL; unsigned int v = p_uint(p); if (!p_expect(p, ')')) return NULL; return TConst(v); }
  if (p_match_kw(p, "var")) { if (!p_expect(p, '(')) return NULL; char * name = p_ident(p); if (!name) return NULL; if (!p_expect(p, ')')) return NULL; return TVar(name); }
  if (p_match_kw(p, "plus")) { if (!p_expect(p, '(')) return NULL; struct expr_int * a = parse_int_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_int * b = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TBinOp(T_PLUS, a, b); }
  if (p_match_kw(p, "minus")) { if (!p_expect(p, '(')) return NULL; struct expr_int * a = parse_int_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_int * b = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TBinOp(T_MINUS, a, b); }
  if (p_match_kw(p, "mul")) { if (!p_expect(p, '(')) return NULL; struct expr_int * a = parse_int_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_int * b = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TBinOp(T_MUL, a, b); }
  return NULL;
}

static struct expr_bool * parse_bool_expr(struct parse *p) {
  p_skip(p);
  if (p_match_kw(p, "true")) return TTrue();
  if (p_match_kw(p, "false")) return TFalse();
  if (p_match_kw(p, "lt")) { if (!p_expect(p, '(')) return NULL; struct expr_int * a = parse_int_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_int * b = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TCmp(T_LT, a, b); }
  if (p_match_kw(p, "gt")) { if (!p_expect(p, '(')) return NULL; struct expr_int * a = parse_int_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_int * b = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TCmp(T_GT, a, b); }
  if (p_match_kw(p, "le")) { if (!p_expect(p, '(')) return NULL; struct expr_int * a = parse_int_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_int * b = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TCmp(T_LE, a, b); }
  if (p_match_kw(p, "ge")) { if (!p_expect(p, '(')) return NULL; struct expr_int * a = parse_int_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_int * b = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TCmp(T_GE, a, b); }
  if (p_match_kw(p, "eq")) { if (!p_expect(p, '(')) return NULL; struct expr_int * a = parse_int_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_int * b = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TCmp(T_EQ, a, b); }
  if (p_match_kw(p, "ne")) { if (!p_expect(p, '(')) return NULL; struct expr_int * a = parse_int_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_int * b = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TCmp(T_NE, a, b); }
  if (p_match_kw(p, "and")) { if (!p_expect(p, '(')) return NULL; struct expr_bool * a = parse_bool_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_bool * b = parse_bool_expr(p); if (!p_expect(p, ')')) return NULL; return TPropBinOp(T_AND, a, b); }
  if (p_match_kw(p, "or")) { if (!p_expect(p, '(')) return NULL; struct expr_bool * a = parse_bool_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_bool * b = parse_bool_expr(p); if (!p_expect(p, ')')) return NULL; return TPropBinOp(T_OR, a, b); }
  if (p_match_kw(p, "imply")) { if (!p_expect(p, '(')) return NULL; struct expr_bool * a = parse_bool_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_bool * b = parse_bool_expr(p); if (!p_expect(p, ')')) return NULL; return TPropBinOp(T_IMPLY, a, b); }
  if (p_match_kw(p, "iff")) { if (!p_expect(p, '(')) return NULL; struct expr_bool * a = parse_bool_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_bool * b = parse_bool_expr(p); if (!p_expect(p, ')')) return NULL; return TPropBinOp(T_IFF, a, b); }
  if (p_match_kw(p, "not")) { if (!p_expect(p, '(')) return NULL; struct expr_bool * a = parse_bool_expr(p); if (!p_expect(p, ')')) return NULL; return TPropUnOp(T_NOT, a); }
  if (p_match_kw(p, "forall")) { if (!p_expect(p, '(')) return NULL; char * name = p_ident(p); if (!p_expect(p, ',')) return NULL; struct expr_bool * a = parse_bool_expr(p); if (!p_expect(p, ')')) return NULL; return TQuant(T_FORALL, name, a); }
  if (p_match_kw(p, "exists")) { if (!p_expect(p, '(')) return NULL; char * name = p_ident(p); if (!p_expect(p, ',')) return NULL; struct expr_bool * a = parse_bool_expr(p); if (!p_expect(p, ')')) return NULL; return TQuant(T_EXISTS, name, a); }
  return NULL;
}

static struct cmd * parse_cmd(struct parse *p) {
  p_skip(p);
  if (p_match_kw(p, "asgn")) { if (!p_expect(p, '(')) return NULL; char * name = p_ident(p); if (!p_expect(p, ',')) return NULL; struct expr_int * r = parse_int_expr(p); if (!p_expect(p, ')')) return NULL; return TAsgn(name, r); }
  if (p_match_kw(p, "skip")) { if (!p_expect(p, '(')) return NULL; if (!p_expect(p, ')')) return NULL; return TSkip(); }
  if (p_match_kw(p, "seq")) { if (!p_expect(p, '(')) return NULL; struct cmd * a = parse_cmd(p); if (!p_expect(p, ',')) return NULL; struct cmd * b = parse_cmd(p); if (!p_expect(p, ')')) return NULL; return TSeq(a, b); }
  if (p_match_kw(p, "if")) { if (!p_expect(p, '(')) return NULL; struct expr_bool * cond = parse_bool_expr(p); if (!p_expect(p, ',')) return NULL; struct cmd * l = parse_cmd(p); if (!p_expect(p, ',')) return NULL; struct cmd * r = parse_cmd(p); if (!p_expect(p, ')')) return NULL; return TIf(cond, l, r); }
  if (p_match_kw(p, "while")) { if (!p_expect(p, '(')) return NULL; struct expr_bool * inv = parse_bool_expr(p); if (!p_expect(p, ',')) return NULL; struct expr_bool * cond = parse_bool_expr(p); if (!p_expect(p, ',')) return NULL; struct cmd * body = parse_cmd(p); if (!p_expect(p, ')')) return NULL; return TWhile(inv, cond, body); }
  return NULL;
}

static int parse_prog(struct parse *p, struct full_annotated_cmd * out) {
  if (!p_match_kw(p, "prog")) return 0;
  if (!p_expect(p, '(')) return 0;
  struct expr_bool * req = parse_bool_expr(p);
  if (!p_expect(p, ',')) return 0;
  struct cmd * c = parse_cmd(p);
  if (!p_expect(p, ',')) return 0;
  struct expr_bool * ens = parse_bool_expr(p);
  if (!p_expect(p, ')')) return 0;
  out->require = req;
  out->ensure = ens;
  out->c = *c;
  return 1;
}

static struct expr_bool * hl_parse_bool_primary2(struct parse *p);
static struct expr_int * hl_parse_int_factor2(struct parse *p);
static struct expr_int * hl_parse_int_expr(struct parse *p);

static int hl_match_sym(struct parse *p, const char *sym) {
  p_skip(p);
  size_t j = 0;
  size_t start = p->i;
  while (sym[j] != '\0') {
    if (p->s[p->i] != sym[j]) { p->i = start; return 0; }
    p->i++; j++;
  }
  return 1;
}

static struct expr_int * hl_parse_int_factor2(struct parse *p) {
  p_skip(p);
  if (p_expect(p, '(')) {
    struct expr_int *e = hl_parse_int_expr(p);
    if (!e) return NULL;
    if (!p_expect(p, ')')) return NULL;
    return e;
  }
  p_skip(p);
  if (p->s[p->i] >= '0' && p->s[p->i] <= '9') {
    unsigned int v = p_uint(p);
    return TConst(v);
  }
  char * id = p_ident(p);
  if (id) return TVar(id);
  return NULL;
}

static struct expr_int * hl_parse_int_factor(struct parse *p) {
  p_skip(p);
  if (p_expect(p, '(')) { struct expr_int *e = NULL; e = hl_parse_int_factor(p); if (!e) e = NULL; if (!e) { return NULL; } if (!p_expect(p, ')')) return NULL; return e; }
  p_skip(p);
  if (p->s[p->i] >= '0' && p->s[p->i] <= '9') {
    unsigned int v = p_uint(p);
    return TConst(v);
  }
  char * id = p_ident(p);
  if (id) return TVar(id);
  return NULL;
}

static struct expr_int * hl_parse_int_term(struct parse *p) {
  struct expr_int * left = hl_parse_int_factor2(p);
  if (!left) return NULL;
  for (;;) {
    p_skip(p);
    if (hl_match_sym(p, "*")) {
      struct expr_int * right = hl_parse_int_factor2(p);
      if (!right) return NULL;
      left = TBinOp(T_MUL, left, right);
      continue;
    }
    break;
  }
  return left;
}

static struct expr_int * hl_parse_int_expr(struct parse *p) {
  struct expr_int * left = hl_parse_int_term(p);
  if (!left) return NULL;
  for (;;) {
    p_skip(p);
    if (hl_match_sym(p, "+")) {
      struct expr_int * right = hl_parse_int_term(p);
      if (!right) return NULL;
      left = TBinOp(T_PLUS, left, right);
      continue;
    }
    if (hl_match_sym(p, "-")) {
      struct expr_int * right = hl_parse_int_term(p);
      if (!right) return NULL;
      left = TBinOp(T_MINUS, left, right);
      continue;
    }
    break;
  }
  return left;
}

static struct expr_bool * hl_parse_bool_primary(struct parse *p) {
  p_skip(p);
  if (p_match_kw(p, "true")) return TTrue();
  if (p_match_kw(p, "false")) return TFalse();
  if (p_expect(p, '(')) { struct expr_bool * b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; b = NULL; struct expr_bool * inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; inner = NULL; if (!p_expect(p, ')')) return NULL; return inner; }
  struct expr_int * left = hl_parse_int_expr(p);
  if (!left) return NULL;
  if (hl_match_sym(p, "==")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_EQ, left, right); }
  if (hl_match_sym(p, "!=")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_NE, left, right); }
  if (hl_match_sym(p, "<=")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_LE, left, right); }
  if (hl_match_sym(p, ">=")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_GE, left, right); }
  if (hl_match_sym(p, "<")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_LT, left, right); }
  if (hl_match_sym(p, ">")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_GT, left, right); }
  if (hl_match_sym(p, "=")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_EQ, left, right); }
  return NULL;
}

static struct expr_bool * hl_parse_bool_not(struct parse *p) {
  p_skip(p);
  if (hl_match_sym(p, "!")) { struct expr_bool * a = hl_parse_bool_not(p); if (!a) return NULL; return TPropUnOp(T_NOT, a); }
  return hl_parse_bool_primary2(p);
}

static struct expr_bool * hl_parse_bool_and(struct parse *p) {
  struct expr_bool * left = hl_parse_bool_not(p);
  if (!left) return NULL;
  for (;;) {
    if (hl_match_sym(p, "&&")) { struct expr_bool * right = hl_parse_bool_not(p); if (!right) return NULL; left = TPropBinOp(T_AND, left, right); continue; }
    break;
  }
  return left;
}

static struct expr_bool * hl_parse_bool_or(struct parse *p) {
  struct expr_bool * left = hl_parse_bool_and(p);
  if (!left) return NULL;
  for (;;) {
    if (hl_match_sym(p, "||")) { struct expr_bool * right = hl_parse_bool_and(p); if (!right) return NULL; left = TPropBinOp(T_OR, left, right); continue; }
    break;
  }
  return left;
}

static struct expr_bool * hl_parse_bool_imply(struct parse *p) {
  struct expr_bool * left = hl_parse_bool_or(p);
  if (!left) return NULL;
  for (;;) {
    if (hl_match_sym(p, "->")) { struct expr_bool * right = hl_parse_bool_or(p); if (!right) return NULL; left = TPropBinOp(T_IMPLY, left, right); continue; }
    break;
  }
  return left;
}

static struct expr_bool * hl_parse_bool_iff(struct parse *p) {
  struct expr_bool * left = hl_parse_bool_imply(p);
  if (!left) return NULL;
  for (;;) {
    if (hl_match_sym(p, "<->")) { struct expr_bool * right = hl_parse_bool_imply(p); if (!right) return NULL; left = TPropBinOp(T_IFF, left, right); continue; }
    break;
  }
  return left;
}

static struct expr_bool * hl_parse_bool(struct parse *p) { return hl_parse_bool_iff(p); }

static void hl_skip_line(struct parse *p) {
  while (p->s[p->i] && p->s[p->i] != '\n') p->i++;
  if (p->s[p->i] == '\n') p->i++;
}

static char * str_trim_copy(const char *s) {
  size_t a = 0; while (s[a] == ' ' || s[a] == '\t' || s[a] == '\r' || s[a] == '\n') a++;
  size_t b = strlen(s);
  while (b > a && (s[b-1] == ' ' || s[b-1] == '\t' || s[b-1] == '\r' || s[b-1] == '\n')) b--;
  size_t n = b - a; char * r = (char*)malloc(n + 1); memcpy(r, s + a, n); r[n] = '\0'; return r;
}

static struct expr_int * parse_int_expr_text(const char *text) {
  struct parse q; q.s = text; q.i = 0; return hl_parse_int_expr(&q);
}

static struct expr_bool * hl_parse_cmp_text(const char *text) {
  const char *ops[] = {"==", "!=", "<=", ">=", "<", ">", "="};
  enum CmpType op_map[] = {T_EQ, T_NE, T_LE, T_GE, T_LT, T_GT, T_EQ};
  for (int k = 0; k < 7; k++) {
    const char *pos = strstr(text, ops[k]);
    if (pos) {
      size_t L = (size_t)(pos - text);
      char * left = (char*)malloc(L + 1); memcpy(left, text, L); left[L] = '\0';
      char * right = strdup(pos + strlen(ops[k]));
      char * tl = str_trim_copy(left);
      char * tr = str_trim_copy(right);
      free(left); free(right);
      struct expr_int * a = parse_int_expr_text(tl);
      struct expr_int * b = parse_int_expr_text(tr);
      free(tl); free(tr);
      if (a && b) return TCmp(op_map[k], a, b);
    }
  }
  return NULL;
}

static struct expr_bool * hl_parse_bool_text(const char *text) {
  struct parse q; q.s = text; q.i = 0; struct expr_bool * b = hl_parse_bool(&q); if (b) return b;
  const char *andpos = strstr(text, "&&");
  if (andpos) {
    size_t L = (size_t)(andpos - text);
    char * left = (char*)malloc(L + 1); memcpy(left, text, L); left[L] = '\0';
    char * right = strdup(andpos + 2);
    struct expr_bool * a = hl_parse_bool_text(left);
    struct expr_bool * c = hl_parse_bool_text(right);
    free(left); free(right);
    if (a && c) return TPropBinOp(T_AND, a, c);
  }
  return hl_parse_cmp_text(text);
}

static struct expr_bool * hl_parse_bool_primary2(struct parse *p) {
  p_skip(p);
  if (p_match_kw(p, "true")) return TTrue();
  if (p_match_kw(p, "false")) return TFalse();
  if (p_expect(p, '(')) {
    struct expr_bool *inner = hl_parse_bool(p);
    if (!inner) return NULL;
    if (!p_expect(p, ')')) return NULL;
    return inner;
  }
  struct expr_int * left = hl_parse_int_expr(p);
  if (!left) return NULL;
  if (hl_match_sym(p, "==")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_EQ, left, right); }
  if (hl_match_sym(p, "!=")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_NE, left, right); }
  if (hl_match_sym(p, "<=")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_LE, left, right); }
  if (hl_match_sym(p, ">=")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_GE, left, right); }
  if (hl_match_sym(p, "<")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_LT, left, right); }
  if (hl_match_sym(p, ">")) { struct expr_int * right = hl_parse_int_expr(p); if (!right) return NULL; return TCmp(T_GT, left, right); }
  return NULL;
}

static struct cmd * seq_append(struct cmd *a, struct cmd *b) {
  if (!a) return b;
  return TSeq(a, b);
}

static struct cmd * hl_parse_cmd(struct parse *p, struct expr_bool **pending_inv) {
  p_skip(p);
  printf("[hl] cmd at '%c'\n", p->s[p->i] ? p->s[p->i] : '#');
  if (p->s[p->i] == '/' && p->s[p->i+1] == '/' && p->s[p->i+2] == '@') {
    p->i += 3;
    p_skip(p);
    if (p_match_kw(p, "inv")) { p_skip(p); struct expr_bool * inv = hl_parse_bool(p); *pending_inv = inv; hl_skip_line(p); return NULL; }
    hl_skip_line(p);
    return NULL;
  }
  if (p_match_kw(p, "skip")) { p_expect(p, ';'); return TSkip(); }
  if (p_match_kw(p, "while")) {
    if (!p_expect(p, '(')) return NULL;
    struct expr_bool * cond = hl_parse_bool(p);
    if (!p_expect(p, ')')) return NULL;
    p_match_kw(p, "do");
    if (!p_expect(p, '{')) return NULL;
    struct cmd * body = NULL;
    for (;;) {
      p_skip(p);
      if (p_expect(p, '}')) break;
      struct cmd * one = hl_parse_cmd(p, pending_inv);
      if (one) body = seq_append(body, one);
    }
    struct expr_bool * inv = *pending_inv ? *pending_inv : TTrue();
    *pending_inv = NULL;
    printf("[hl] while inv chosen: ");
    print_expr_bool(inv);
    printf("\n");
    return TWhile(inv, cond, body ? body : TSkip());
  }
  char * name = p_ident(p);
  if (name) {
    p_skip(p);
    printf("[hl] ident %s, next char '%c'\n", name, p->s[p->i] ? p->s[p->i] : '#');
    if (p->s[p->i] == '=' && p->s[p->i+1] != '=') {
      p->i++;
      struct expr_int * rhs = hl_parse_int_expr(p);
      p_expect(p, ';');
      printf("[hl] asgn %s\n", name);
      return TAsgn(name, rhs);
    }
  }
  return NULL;
}

static int parse_hl_prog(struct parse *p, struct full_annotated_cmd * out) {
  struct expr_bool * require = NULL;
  struct expr_bool * ensure = NULL;
  struct expr_bool * pending_inv = NULL;
  struct cmd * seq = NULL;
  /* pre-scan all lines to collect require/ensure/inv ahead of commands */
  {
    size_t k = 0;
    while (p->s[k]) {
      /* move to start of line */
      while (p->s[k] == ' ' || p->s[k] == '\t' || p->s[k] == '\r') k++;
      if (p->s[k] == '/' && p->s[k+1] == '/' && p->s[k+2] == '@') {
        k += 3;
        while (p->s[k] == ' ' || p->s[k] == '\t') k++;
        if (strncmp(p->s + k, "require", 7) == 0) {
          k += 7; while (p->s[k] == ' ' || p->s[k] == '\t') k++;
          size_t start = k; while (p->s[k] && p->s[k] != '\n' && p->s[k] != '\r') k++;
          size_t len = k - start; char * buf = (char*)malloc(len + 1); memcpy(buf, p->s + start, len); buf[len] = '\0';
          struct expr_bool * b = hl_parse_bool_text(buf);
          if (b) { require = b; printf("[hl] set require (pre): "); PrintExprBool(require); printf("\n"); }
          free(buf);
        } else if (strncmp(p->s + k, "ensure", 6) == 0) {
          k += 6; while (p->s[k] == ' ' || p->s[k] == '\t') k++;
          size_t start = k; while (p->s[k] && p->s[k] != '\n' && p->s[k] != '\r') k++;
          size_t len = k - start; char * buf = (char*)malloc(len + 1); memcpy(buf, p->s + start, len); buf[len] = '\0';
          struct expr_bool * b = hl_parse_bool_text(buf);
          if (b) { ensure = b; printf("[hl] set ensure (pre): "); PrintExprBool(ensure); printf("\n"); }
          free(buf);
        } else if (strncmp(p->s + k, "inv", 3) == 0) {
          k += 3; while (p->s[k] == ' ' || p->s[k] == '\t') k++;
          size_t start = k; while (p->s[k] && p->s[k] != '\n' && p->s[k] != '\r') k++;
          size_t len = k - start; char * buf = (char*)malloc(len + 1); memcpy(buf, p->s + start, len); buf[len] = '\0';
          struct expr_bool * b = hl_parse_bool_text(buf);
          if (b) { pending_inv = b; printf("[hl] set inv (pre): "); PrintExprBool(pending_inv); printf("\n"); }
          free(buf);
        }
      }
      while (p->s[k] && p->s[k] != '\n') k++;
      if (p->s[k] == '\n') k++;
    }
  }
  for (;;) {
    p_skip(p);
    if (!p->s[p->i]) break;
    printf("[hl] top at '%c'\n", p->s[p->i] ? p->s[p->i] : '#');
    if (p->s[p->i] == '/' && p->s[p->i+1] == '/' && p->s[p->i+2] == '@') {
      p->i += 3;
      p_skip(p);
      if (p_match_kw(p, "require")) { p_skip(p); require = hl_parse_bool(p); printf("[hl] set require: "); PrintExprBool(require); printf("\n"); hl_skip_line(p); continue; }
      if (p_match_kw(p, "ensure")) {
        p_skip(p);
        size_t start = p->i;
        size_t end = start;
        while (p->s[end] && p->s[end] != '\n' && p->s[end] != '\r') end++;
        size_t len = end - start;
        char * buf = (char *) malloc(len + 1);
        memcpy(buf, p->s + start, len);
        buf[len] = '\0';
        ensure = hl_parse_bool_text(buf);
        printf("[hl] set ensure: "); PrintExprBool(ensure); printf("\n");
        free(buf);
        hl_skip_line(p);
        continue;
      }
      if (p_match_kw(p, "inv")) {
        p_skip(p);
        size_t start = p->i;
        size_t end = start;
        while (p->s[end] && p->s[end] != '\n' && p->s[end] != '\r') end++;
        size_t len = end - start;
        char * buf = (char *) malloc(len + 1);
        memcpy(buf, p->s + start, len);
        buf[len] = '\0';
        pending_inv = hl_parse_bool_text(buf);
        printf("[hl] set inv: "); PrintExprBool(pending_inv); printf("\n");
        free(buf);
        hl_skip_line(p);
        continue;
      }
      hl_skip_line(p);
      continue;
    }
    {
      int handled = 0;
      size_t j = p->i;
      while (p->s[j] && p->s[j] != '\n' && p->s[j] != '\r') {
        if (p->s[j] == '/' && p->s[j+1] == '/' && p->s[j+2] == '@') {
          size_t old = p->i;
          p->i = j + 3;
          p_skip(p);
          if (p_match_kw(p, "require")) { p_skip(p); require = hl_parse_bool_text(p->s + p->i); printf("[hl] set require: "); PrintExprBool(require); printf("\n"); hl_skip_line(p); handled = 1; break; }
          if (p_match_kw(p, "ensure")) {
            p_skip(p);
            size_t start = p->i; size_t end = start; while (p->s[end] && p->s[end] != '\n' && p->s[end] != '\r') end++;
            size_t len = end - start; char * buf = (char *) malloc(len + 1); memcpy(buf, p->s + start, len); buf[len] = '\0';
            ensure = hl_parse_bool_text(buf);
            printf("[hl] set ensure: "); PrintExprBool(ensure); printf("\n");
            free(buf);
            hl_skip_line(p); handled = 1; break;
          }
          if (p_match_kw(p, "inv")) {
            p_skip(p);
            size_t start = p->i; size_t end = start; while (p->s[end] && p->s[end] != '\n' && p->s[end] != '\r') end++;
            size_t len = end - start; char * buf = (char *) malloc(len + 1); memcpy(buf, p->s + start, len); buf[len] = '\0';
            pending_inv = hl_parse_bool_text(buf);
            printf("[hl] set inv: "); PrintExprBool(pending_inv); printf("\n");
            free(buf);
            hl_skip_line(p); handled = 1; break;
          }
          p->i = old;
          break;
        }
        j++;
      }
      if (handled) continue;
    }
    struct cmd * c = hl_parse_cmd(p, &pending_inv);
    if (c) printf("[hl] parsed cmd type %d\n", c->t);
    if (c) seq = seq_append(seq, c);
    else {
      size_t save = p->i;
      char * name = p_ident_anywhere(p);
      if (name) {
        p_skip(p);
        if (p->s[p->i] == '=' && p->s[p->i+1] != '=') {
          p->i++;
          struct expr_int * rhs = hl_parse_int_expr(p);
          p_expect(p, ';');
          struct cmd * ac = TAsgn(name, rhs);
          seq = seq_append(seq, ac);
          printf("[hl] fallback asgn %s\n", name);
          continue;
        }
      }
      p->i = save;
      p_skip(p);
      if (p->s[p->i] && p->s[p->i] != '}') hl_skip_line(p);
    }
  }
  if (!require) require = TTrue();
  if (!ensure) ensure = TTrue();
  if (!seq) seq = TSkip();
  out->require = require;
  out->ensure = ensure;
  out->c = *seq;
  return 1;
}

int main(int argc, char **argv) {
  if (argc >= 2) {
    char * text = read_all(argv[1]);
    if (!text) { printf("Failed to read input file.\n"); return 1; }
    struct parse p; p.s = text; p.i = 0;
    struct full_annotated_cmd prog_out;
    int ok = 0;
    p_skip(&p);
    if (p_match_kw(&p, "prog")) { p.i = 0; ok = parse_prog(&p, &prog_out); }
    else { p.i = 0; ok = parse_hl_prog(&p, &prog_out); }
    if (!ok) { printf("Parse error.\n"); free(text); return 1; }
    struct vc_list * vcs = GenerateVCs(&prog_out);
    PrintProgram(&prog_out);
    PrintVCs(vcs);
    free(text);
    return 0;
  }
  run_tests();
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

  return 0;
}
