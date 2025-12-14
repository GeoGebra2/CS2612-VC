# 带标注 SimpleWhile：符号执行与 VC 生成

## 项目简介
- 输入：带标注 SimpleWhile 程序的抽象语法树（或 `Test` 目录中的文本用例）。
- 输出：程序的验证条件（VC）列表，打印到控制台。
- 主要文件：`lang.h`（AST 与接口）、`lang.c`（实现与解析、打印）。
- 核心流程：`GenerateVCs` 调用 `P2Q` 逐条命令回推弱前置，并追加循环初始化/保持 VC，最后将整体前置推出确保条件。

## 编译
- Windows 已提供可执行文件：`lang.exe`
- 如需重新构建：`gcc -std=c99 -O2 lang.c -o lang.exe`

## 运行测试
- 运行Test中全部示例：`./lang.exe Test`
- 运行Test中单个用例：`./lang.exe Test\sample_hl_while_01.txt`
- 程序会打印程序的 `{require}`/命令/`{ensure}` 以及逐条 `VC i:` 列表。
- 运行程序内置用例：`./lang.exe`。程序会输出内置构造的3个测试用例的 VC 列表。

## 测试用例
- `Test/`中构建了22 个示例用例（`.txt`），包含条件、循环不变式与后置条件。测试了顺序执行语句，if语句，while语句，while和if的嵌套，以及while和while的嵌套等多种情况下的程序表现。
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