/*
 * 带标注 SimpleWhile 语言的抽象语法定义与构造接口
 *
 * 主要内容：
 * - 整数表达式（expr_int）、布尔/断言表达式（expr_bool）、命令（cmd）以及完整带注解程序（full_annotated_cmd）
 * - 构造这些语法节点的辅助函数（在 lang.c 中实现）
 * - 符号执行与弱前条件（WP）计算、验证条件（VC）生成与打印接口
 *
 * 约定：
 * - 所有节点均通过提供的构造函数创建；内存由 malloc 分配，调用者需根据需要管理生命周期
 * - while 语句节点内含循环不变式 inv，VC 生成将基于 inv 产生初始化/保持/退出三类 VC
 */
#ifndef LANG_H_INCLUDED
#define LANG_H_INCLUDED

#include <stdio.h>
#include <stdlib.h>

/*
 * 整数二元运算类型：加、减、乘
 */
enum IntBinOpType {
  T_PLUS,
  T_MINUS,
  T_MUL,
  T_DIV
};

/*
 * 比较运算类型：<, >, <=, >=, ==, !=
 */
enum CmpType {
  T_LT,
  T_GT,
  T_LE,
  T_GE,
  T_EQ,
  T_NE
};

/*
 * 命题二元运算：与、或、蕴含、等价
 */
enum PropBinOpType {
  T_AND,
  T_OR,
  T_IMPLY,
  T_IFF
};

/*
 * 命题一元运算：非
 */
enum PropUnOpType {
  T_NOT
};

/*
 * 量词类型：存在、全称
 */
enum QuantType {
  T_EXISTS,
  T_FORALL
};

/*
 * 整数表达式种类：常量、变量、二元算术表达式
 */
enum ExprIntType {
  T_CONST = 0,
  T_VAR,
  T_BINOP
};

/*
 * 布尔/断言表达式种类：true/false、比较、命题二元/一元、量词
 */
enum ExprBoolType { // 和断言使用同一个结构
  T_TRUE = 0,
  T_FALSE,
  T_CMP,
  T_PROP_BINOP,
  T_PROP_UNOP,
  T_QUANT
};

/*
 * 命令类型：赋值、跳过、顺序、条件、循环（带不变式）
 */
enum CmdType {
  T_ASGN = 0,
  T_SKIP,
  T_SEQ,
  T_IF,
  T_WHILE
};

/*
 * 整数表达式节点
 * - CONST: 无符号整数常量
 * - VAR: 变量名字符串
 * - BINOP: 算术二元运算（左右子表达式）
 */
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

/*
 * 布尔/断言表达式节点
 * - TRUE/FALSE: 常量命题
 * - CMP: 比较左右整数表达式
 * - PROP_BINOP: 命题二元运算（左右子断言）
 * - PROP_UNOP: 命题一元运算（否定）
 * - QUANT: 量词绑定变量名，对内部断言施加量词
 */
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

/*
 * 命令节点
 * - ASGN: 赋值，左边为变量名，右边为整数表达式
 * - SKIP: 跳过（空操作）
 * - SEQ: 顺序执行（左；右）
 * - IF: 条件分支（条件、then、else）
 * - WHILE: 循环（不变式、条件、循环体）
 */
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

/*
 * 完整带注解程序
 * - require: 前置条件（入口断言）
 * - ensure: 后置条件（出口断言）
 * - c: 主体命令 AST
 */
struct full_annotated_cmd {
  struct expr_bool * require;
  struct expr_bool * ensure;
  struct cmd c;
};

/*
 * 构造辅助函数：创建各类 AST 节点
 * 注意：字符串参数按指针传递，不进行复制；如需独立副本请自行 clone
 */
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

/*
 * 深拷贝与替换接口：
 * - CloneExprInt/CloneExprBool: 递归复制表达式节点
 * - SubstInt: 在整数表达式中将变量 x 替换为表达式 e
 * - SubstBool: 在断言中进行替换，注意量词绑定变量的捕获规避
 */
struct expr_int * CloneExprInt(struct expr_int *);
struct expr_bool * CloneExprBool(struct expr_bool *);
struct expr_int * SubstInt(struct expr_int *, char *, struct expr_int *);
struct expr_bool * SubstBool(struct expr_bool *, char *, struct expr_int *);
/*
 * 正向符号执行与 VC 生成接口：
 * - SP(c, P): 计算命令 c 在入口断言 P 下的最强后条件（符号执行）
 * - GenerateVCsForward(p): 基于不变式等生成 VC 列表（初始化/保持/退出）
 */
struct vc_list;
struct expr_bool * SP(struct cmd *, struct expr_bool *);
struct vc_list * GenerateVCsForward(struct full_annotated_cmd *);
struct expr_bool * SP(struct cmd *, struct expr_bool *);
struct vc_list * GenerateVCsForward(struct full_annotated_cmd *);
/*
 * 打印接口：
 * - PrintExprInt/PrintExprBool: 打印整数表达式与断言表达式（中缀格式）
 * - PrintVCs: 逐条打印 VC 列表
 * - PrintCmd: 打印命令 AST（包含 while 的不变式）
 * - PrintProgram: 打印完整的带注解程序（require / cmd / ensure）
 */
void PrintExprInt(struct expr_int *);
void PrintExprBool(struct expr_bool *);
void PrintVCs(struct vc_list *);
void PrintCmd(struct cmd *);
void PrintProgram(struct full_annotated_cmd *);

struct expr_int * PrimeExprInt(struct expr_int *);
struct expr_bool * PrimeExprBool(struct expr_bool *);

#endif // LANG_H_INCLUDED
