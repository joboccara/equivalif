```mermaid
flowchart TD
    Start --Source--> Lexer
    subgraph Parser
        Lexer --> ASTBuilder
    end
    ASTBuilder --AST--> Evaluator
    Evaluator --Truth table-->Comparator
    Comparator --Truth table diff-->End
```