# 带标注 SimpleWhile：符号执行与 VC 生成

## 项目简介
- 输入：带标注 SimpleWhile 程序的抽象语法树（或 `Test` 目录中的文本用例）。
- 输出：程序的验证条件（VC）列表，打印到控制台。
- 主要文件：`lang.h`（AST 与接口）、`lang.c`（实现与解析、打印）。
- 核心流程：`GenerateVCs` 调用 `P2Q` 逐条命令进行符号执行，最后用符号执行的最终结果推出确保条件。

## 编译
- Windows 已提供可执行文件：`lang.exe`
- 如需重新构建：`gcc -std=c99 -O2 lang.c -o lang.exe`

## 注意
- 如果想要增加测试用例，建议在lang.c中仿照现有的测试用例构造AST，以避免程序到AST的解析不正确造成错误的VC生成（程序到AST的解析并非我们组任务范围之内的要求，所以我们在这一块内容上并没有进行过多的测试）。
- 我们所使用测试示例仅用于验证符号执行过程和VC生成的正确性，但并不保证循环不变量的正确性，即我们按照任务要求保证能够按照给出的require，ensure以及inv正确生成VC，但不检验相应的霍尔三元组能否被正确证明。

## 测试用例
- `Test/`中构建了22 个示例用例（`.txt`），包含条件、循环不变式与后置条件。测试了顺序执行语句，空语句，if语句，while语句，while和if的嵌套(e.g. 18)，以及双重while循环(e.g. 19)等多种情况下的程序表现。
- `lang.c`内置了3个示例用例，以AST形式作为输入。测试了if语句，while和if嵌套，以及条件中含有谓词和量词情况下的程序表现。

## 使用说明
### 若输入为AST
参考lang.c中3个测试用例(`Absolute`, `04`, `05`)的输入格式，可在`lang.c`的`main`函数中直接调用相关函数进行符号执行和VC生成。
- 使用`GenerateVCs()`函数进行符号执行和VC生成。
- 使用`PrintProgram()`函数打印输入的程序
- 使用`PrintVCs()`函数打印得到的VC。

使用范例如下，其中`p04`为`full_annotated_cmd`类型：
```
struct vc_list * vcs04 = GenerateVCs(&p04);
printf("\n=== Program 04: While with If AST ===\n");
PrintProgram(&p04);
printf("\n=== Program 04 VCs ===\n");
PrintVCs(vcs04);
```
### 若输入为完整程序
参考Test文件夹中22个测试用例`sample_hl_while_xx.txt`，可直接使用编译好的`lang.exe`进行符号执行和VC生成。

使用命令行命令：
```
./lang.exe Test\sample_hl_while_xx.txt
```
