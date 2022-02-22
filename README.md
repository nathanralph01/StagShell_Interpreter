# Haskell Interpreter
In this project, we created a basic interpreter for Haskell, called StagShell.

StagShell is an expression-based language with eager evaluations. The language supports unique types of values and epression grammar, which is shown in 
A2Types.hs.

The supported expression include:
  Literal value: Expressions invloving Literal takes a subexpression that evaluates to a Literal Value.
  
  Plus, Times: Expressions involving Plus/Times take two subexpressions which evaluate to numbers, and produce a number corresponding to their sum/product.
  
  Equal: Takes two subexpressions and returns a Boolean value with respect to their equivalence.
  
  Cons: Takes two subexpressions and returns a Pair out of the resulting expressions.
  
  
  First/Rest: Takes a subexpression, and returns the first/second element if the expression is a Pair.
  
  If expressions: Takes three subexpressions (If cond expr alt). The first being the condition, the second being an expression which is evaluated if the condition is true,
  and the third an expression which is evaluated if the condition is not true.
  
  Variable lookup: Given an identifier, this expression will evaluate to its correspoding value in the environment.
  
  Function expressions: Takes a list of identifiers, a body expression, and evaluates to a closure.
  
  Function application: Takes a function expression and a list of arguemnts. Applies the expression to each of the arguments.

*Note: This project was created as an assignment for CSC324 at University of Toronto Mississauga. For students looking at this repo while taking the course,
do so at your own risk. Do not plagarize, Markus will detect you.
