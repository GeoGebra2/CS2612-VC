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
- 运行全部示例：`./lang.exe Test`
- 运行单个用例：`./lang.exe Test\sample_hl_while_01.txt`
- 程序会打印程序的 `{require}`/命令/`{ensure}` 以及逐条 `VC i:` 列表。
- 运行程序内置用例：`./lang.exe`。程序会输出内置构造的五个测试用例的 VC 列表。

## 目录说明
- `Test/`：12 个示例用例（`.txt`），包含条件、循环不变式与后置条件。
