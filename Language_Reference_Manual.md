# Language Reference Manual
## Comments and Whitespace
### Single-line Comments
Single-line comments are denoted with two forward-slashes. 
```
// single-line comment
```

### Multi-line Comments
Multi-line comments are written in between ```/*``` and ```*/```. Multi-line comments can be nested as long as the opening ```/*``` and closing ```*/``` are all matched.
```
/* multi-line comment */

/* nested /* multi-line */ comment */
```

### Whitespace
Whitespace, including newline characters, tabs, and spaces, is used only to separate tokens and is otherwise ignored by the Shoo compiler. 

## Data Types

There is no automatic type promotion in Shoo. For types that can be casted into other types, built-in functions (discussed in section 3.9) exist to facilitate this.

### Primitive Data Types
```int``` is an architecture-dependent signed 32-bit integer type that is made up of a sequence of digits representing a number in base 10. A ```-``` is used to denote negative numbers. It is a right-associative operator.

```float``` is an architecture-dependent signed IEEE 64-bit floating-point type. A ```-``` is used to denote negative numbers. It is a right-associative operator.

```string``` is a sequence of characters in ASCII enclosed in double-quotes and excludes the double-quote ```"``` character. There are no escape sequences. So, for example, you won't be able to print out double-quote characters by escaping the double-quote, but you can print out two single-quotes if you would like something similar to double-quotes.

```bool``` is an expression with a value of true or false.

### The Void Keyword
The type ```void``` has no associated value and can only be used as the return type for functions that return nothing. This is useful for functions which are intended to perform "side-effect" operations only. The return statement can be omitted in this case or can be written as: 
```
return;
```

## Variables
### Variable Naming
 All variable names must follow ```[a-zA-Z][a-zA-Z0-9]*``` and can't be a reserved word (shown below).
 
 true if elif else for while function func void return
 
 int float char string bool array struct
 
 print println scan_line scan_char exit success 
 
 str_of_bool str_of_float str_of_int die string concat 
 
 int_of_float int_of_str float_of_int rand_autoseed 
 
 rand_afterseed

### Scope of Variables
Shoo is a statically scoped language. Variables declared inside blocks, where blocks include functions, for loops, if statements, while loops, and struct definitions, exist only inside the block in which they are declared and override any variables of the same name declared before that block within that block only. Variables outside of all blocks have global scope and thus can be accessed anywhere in the program following their declaration. Multiple variables and/or functions of the same name, even if their types differ, cannot be declared in the same scope. Variable, struct, and function declarations are not visible to statements that precede them so Shoo does not support recursive or mutually recursive functions or struct type definitions.
 
### Variable Declarations
Each variable has a type specified at the time of declaration. The type must be one of the primitive data types specified above or a composite types, which include arrays, structs, and functions.
 
See section 3.4 Arrays and section 3.5 Structs for information about declaring and defining variables of these types.

### Variable Assignment
The equals operator (```=```) is used for assignment. It assigns the value of the expression on the right-hand side to the variable on the left-hand side. The type of the expression on the right-hand side must match the type of the variable on the left-hand side. See section 6.2 Assignment Operator for more information about this operation and its associativity.

## Arrays
An array is a container of primitive types, structs, functions, or other arrays (with caveats, see section 3.4.3 Arrays in Arrays below).

### Declaring Arrays
The declaration and definition of arrays can happen in one statement or two. See 3.4.2 Defining Arrays section below about how to do declare and define arrays in one statement. 

The type of the elements in an array is specified at the time of definition. The size of the array is specified at the time of declaration. Arrays are declared using the keyword array, followed by arrow brackets, which contain the type of the elements in the array, (```<type>```), the array name, and a semi-colon. The semi-colon can be omitted if the array is being declared and defined in one line.
```
/* Declare a variable of type array<int>. It cannot be used until it is initialized. */
array<int> z; 
```

### Defining Arrays
You must define the size of the array when you declare an array. Arrays are fixed in size.
To define an array, you have two options.

The first option is to use the keyword ```new```. Arrays can be initialized with the keyword ```new``` followed by parentheses that enclose the word array, the type of the array in the arrow brackets, and the size of the array in square brackets. If using this way of defining an array, you can make an array of size zero.
```
/* Define the array using keyword "new." You must provide a size in this step. The size goes in the [] brackets. */
array<int> z;
z = new(array<int>[5]); 

// Declare and define an array in one line.
array<int> xy = new(array<int>[5]);

int x = 50;
/* Declare and define an array of type struct. Assume BankAccount has already been declared as a struct type. See section 3.5.2 Variables of Type Struct for information about declaring variables of type struct. */
array<BankAccount> ba = new(array<BankAccount>[x]);
```

The second option is to define the elements of the array explicitly, in which case you do not need to explicitly specify the size. The elements are put in square brackets and are separated by commas. There must be at least one element in the brackets (i.e. the brackets cannot be empty). The last element in the list of elements is not followed by a comma. The whole statement is terminated by a semi-colon. The number of elements you list is the size of the array. 

```
/* Declare an array and specify the elements in the array explicitly during definition all in one statement. */
array<int> x = [5,4,3,2,1];

// Defining the elements in the array explicitly after declaring the array.
array<int> b;
b = [1,2,3];
```

### Arrays in Arrays
To declare an array inside an array, you have two options. You can either define the values of all of the arrays explicitly using the bracket syntax explained in 4.2 Defining Arrays. 
Example:
```
array<array<int>> foo = [
   [5,2,3,4],
   [11,12,13,14]
];

```

Your other option is to declare and definite the outer array using the "new" syntax described in 4.2 Defining Arrays but omitting the size of the inner array. Then, you must define the nested array elements using any defining array technique discussed in 4.2 Defining Arrays. The size of the nested array doesn't have to be the same for all other the arrays.

```
array<array<int>> a = new(array<array<int>>[2]); // Declare and define an array of 2 arrays.

a[0] = new(array<int>[3]); // Define one of the nested arrays to be an array of size 3.
a[1] = new(array<int>[4]); // Define another one of the nested arrays to be an array of size 4.

for (int i = 0; i < 2; i++) {
 for (int j = 0; j < 3; j++) {
  a[i][j] = i * j;
 }
}

println(str_of_int(a[0][0]));
println(str_of_int(a[1][2]));
```

Additionally, you can have a struct with an array as a member so you can achieve nested arrays by having arrays of structs that have arrays as members.

### Accessing Array Elements}
To access an element in an array, you use the variable name for that array followed by square brackets with an integer expression inside the square brackets as the index of the desired element. For nested arrays, square brackets with indices in them are concatenated to get to the element at the desired layer of the nested array. Accessing elements beyond the size of the array causes undefined behavior. 
```
array<int> a = new(array<int>[5]);
a[1+2] = 5;

array<array<int>> foo = [
   [5,2,3,4],
   [11,12,13,14]
];
foo[0][2] = 4;
```

## Structs
### Declaring Structs
A struct is a list of grouped variables, where the variable types can be any primitive type, arrays, functions, or other structs. Structs are defined using the keyword ```struct```, then the name, which must start with a capital letter and then can contain any combination of letters and numbers, all followed by ```{ }``` containing the body of the struct. The body of the struct can only contain variables declarations, which are possibly defined in the same statement, terminated by semi-colons. The variables are optionally initialized to default values, which occur if the optional definition of the member fields are present. These default values cannot be accessed until a variable of the struct type is initialized with ```new```. These default values are fresh everytime a new struct of the type is created (ie. if you have a function evaluated as a default value, it is called anew every time you create a struct of that type). The body of the struct can also be empty, but the ```{ }``` are still mandatory. Example: 
```
struct BadStruct {
  int x;
  x = 4.0; // This is not allowed. Declaration and definition must happen in one line. 
}

struct BankAccount { // Declare the struct.
  float balance = 0.0; // Default values are optional. 
  int ownerId;
}

// Have a function as a default value in a struct.
// See section 3.8 Functions for details about writing functions.
function myFunction(int y) int {
    return y+5;
}
 struct Baz { 
  /* This memeber is of type func that takes an int and returns an int and is
  initialized to myFunction. myFunction must be declared before this statement.*/
  func(int; int) field1 = myFunction; 
  int field2;
}

struct SomeStuff {} // Empty struct

struct Bank { 
  int bankNumber;
  BankAccount a1; // A struct as a member of another struct. BankAccount must
          // be declared before this statement. 
}
```

### Variables of Type Struct
 If the variable is of type struct, the struct type must be declared before the variable of that type is defined. The struct name, without the keyword ```struct```, will be used to identify the type of the variable. Even though the variable is declared, its members cannot be accessed before it is defined, as explained in section 5.3 Defining Variables of Type Struct. 
 ```
 struct BankAccount { // Declare the struct type.
           // This syntax is described above in the section 5.1 Declaring Structs.
  int balance;
  int ownerId;
}
BankAccount myAccount; // Declare a variable of type BankAccount.
```

### Defining Variables of Type Struct
A variable of type struct must be defined in either of two ways before it is accessed. Accessing a struct that has not been defined leads to undefined behavior.

Variables of type struct can be defined with the keyword ```new``` followed by parentheses that enclose the name of the struct. The declaration and definition can happen in one statement or two. 
```
BankAccount myAccount; // Declare a variable of struct type BankAccount.
myAccount = new(BankAccount); // Define the variable of struct type BankAccount.

// You can also declare and define the variable in one line.
BankAccount yourAccount = new(BankAccount);
```

Additionally, variables of type struct can be defined by writing the field name and then assigning it to a value followed by a semi-colon. This definition must have a semi-colon following its closing curly brace.
```
BankAccount myAccount; // Declare a variable of struct type BankAccount.
myAccount = {balance = 0; ownerId = 12345;}; // Define the variable by initializing its default values.
```

### Dot Operator
You can access the element of a struct using the dot operator (```.```). You need to specify the name of the variable that is of the struct type (see 3.5.2 Variables of Type Struct for more information about these types of variables) and the fields within the struct that you would like to access or change. 

The dot operator is left associative. 
```
BankAccount yourAccount = new(BankAccount); // Declare and define a variable of type BankAccount.
yourAccount.balance = 6; // Change the value of the balance member in yourAccount.
print(str_of_int(yourAccount.balance)); // Read the value of the balance in yourAccount using the dot operator.
```

### Destructuring Assignment of Structs
Destructuring assignment is a way of extracting all the fields of a struct at once. In the following code, a struct of some type ```bar``` named ```foo``` is created and then "destructure assigned".
```
// Define a struct Bar.
struct Bar {
  int field1;
  int field2;
  int field3;
}

// Create an instance of Bar named foo.
Bar foo = new(Bar);
foo.field1 = 2;
foo.field2 = 3;
foo.field3 = 4;

// Now destructure assign foo.
{x; y; z;} = foo;

print(x); // will print 2
print(y); // will print 3
print(z); // will print 4
```
The line ```{x; y; z;} = foo;``` contains the destructure assignment syntax. It is a shortcut for copying the fields of a struct instance into variables corresponding to each field in one line. For instance, in the example above, the variable ```x``` now holds a copy of the value of ```field1``` of ```foo``` and similarly with ```y``` for ```field2``` and ```z``` for ```field3```. 

Note that because the destructure assignment syntax is just syntactic sugar for declaring a new variable of the correct type (ie. ```int x = foo.field1```) you cannot take an already declared variable name and then put that variable name in the destructure assignment's curly braces.

```
// Bar and foo defined in the prior code segment
int c = 5;

{c; b; a;} = foo; // this will not work because you are
// essentially redeclaring c in the same scope.

```

## Operators

### Precedence and Associativity
For order of evaluation, our arithmetic operators follow PEMDAS. The modulo operator has the same precedence as multiplication and division. Only the assignment operator (```=```), the NOT operator (```!```), described in 8.7 Logical Operators,and the postfix operators (```++``` and ```--```), described in 6.6 Increment and Decrement Operators, are right associative. The other operators are left associative. 

Because Shoo does not do automatic type promotion, you cannot perform any of the following binary operators on multiple variables of different types (ie. performing arithmetic addition on a variable of type ```int``` and a type ```float``` will not work).

### Assignment Operator
The equal sign (```=```) is used for assignment. It is right associative.

### Arithmetic Operators
```+``` is used for addition in the traditional mathematical sense for ```int``` and ```float``` types. The addition operator can also be used to concatenate strings.

```
// sample usage of the addition operator
int a = 5;
int b = 7;
int ab = a + b; 
print(str_of_int(ab)); // this prints: 12

string one = "hello";
string two = " world";
string result = one + two;
print(result); // this prints: hello world
```

The minus operator (```-```) is used for subtraction in the traditional mathematical sense for ```int``` or ```float``` types only.

Asterisk is used for multiplication in the traditional, mathematical sense for ```int``` or ```float``` types only. 

Forward slash is used for division. That is, when you divide two ```int``` types, you get the quotient and the remainder is discarded without any rounding. If you divide two ```float``` types, the result will be to some decimal point unit. This operator can be applied to ```int``` and ```float``` types only. 

The percent sign (```%```) is used for the modulo operation and can be used for ```int``` and ```float``` types only.

Boolean operators in this language are: ```==``` (checks for equality) , ```!=``` (checks for inequality), ```<```, ```>```, ```>=```, ```<=```. The last four operate on ```int``` and ```float``` types only. and ```!=``` also work for strings, arrays and structs. For strings, these two operators work on string equality, that is, are the two strings the same length and comprised of the same characters in the same order. For structs and arrays however, the two operators check the equality of the memory reference, that is, do the arrays or structs refer to the same underlying array or struct? 

```==``` and ```!=``` can also be used for booleans and boolean expressions that evaluate to true or false.

### Logical Operators
```!``` is used for NOT in boolean expressions. 
```&&``` is used for AND in boolean expressions. 
```||``` is used for OR in boolean expressions.

Note that the OR and AND operators do not have short-circuiting feature, meaning that full expression is evaluated regardless of the truth value of any part of the expression.

### Increment and Decrement Operators
The postfix, increment, and decrement operators are used to add or substract ```1``` for ```int``` types only. 

## Statements

### Simple Statements
Statements are usually terminated with semicolons (```;```). A semi-colon cannot be by itself. It must be preceded by a statement. 

### The For Loops
```for``` loops can be used to specify iteration and looping behavior.

A ```for``` loop statement has a header and a body. The header consists of three expressions separated by semi-colons. Both parts (the header and the body) are mandatory. The header has an initialization statement, a testing condition, an increment/decrement expression.

The initialization statement is evaluated one time only. It is evaluated before you test the test condition for the first time. You can declare variables in this statement or have an expression here (or even any statement). This initialization statement is optional. If you choose to omit the initialization statement, you still must have one semi-colon denoting where it would have ended.

The test condition is tested before you enter the loop the first time and then every time after the increment/decrement expression is evaluated until the test condition is false.

Finally, the increment/decrement expression is evaluated after each loop iteration, before the test condition is tested again. This part is optional. If you choose to omit the increment/decrement expression, you still must have one semi-colon denoting where it would have ended.

The loop body can contain any number of statements, including zero statements. The brackets are required even if the loop body is empty. A loop iteration consists of evaluating the statements in the loop body. 

Example:
```
int sum = 0;
for (int i = 0; i < 10; i = i + 1) {
	sum = sum + i;
}
```
Example with return:
```
function foo() string {
  for (int i = 5; i < 10; i = i + 1) {
    return "hello " + str_of_int(i);
  }
}
```

The initialization statement, test condition, and increment/decrement expression can all be omitted to get an infinite loop. Note, there is no break statement so it is often best to write potentially infinite loops inside of functions so you can use return statements to stop them.

```
for ( ; ; ) {
	// infinite loop
}
```

### The While Loops
While loops repeatedly execute a block of code as long as the test condition is true.
A while loop statement has a header followed by curly braces that contain the body of the while loop, which is the block of code that is executed as long as the condition in the header is true. The header has the keyword while followed by parenthesizes enclosing the condition to test.

Example: 
```
int i = 0;
while (i < 3) {
  println(str_of_int(i));
  i++;
}
```

Similar to the for loops, while loops are infinite when the test condition is left empty.

### The If Statements
The if statement supports conditional execution. The if statement has a boolean expression in parenthesis followed by curly braces for the body. The body of the if statement can be zero or more statements. The if statement can then be followed by any number of optional elif statements, which must have a boolean expression specified inside its parenthesis, just like an if statement, and can contain zero or more statement in its body, which must be enclosed by curly braces as well. The elif statement (or the if statement if elif statements were omitted) can be followed by a single, optional else statement. The else statement does not have a boolean expression in parenthesis. The word "else" is simply followed by the curly braces for its body. 
If the boolean expression for the given statement evaluates to true, then the body associated with that expression is evaluated, and then any following elif/else statements are skipped and the next statement is evaluated. The parentheses and braces are always required. 

Example:
```
if (x > 0) {
	println("x is positive"); // println is a built-in function. 
	// See section 3.9.2 println for more information.
} elif (x == 0) {
	println("x is zero");
} else {
	println("x is negative");
}

if (x == 0) {
  x = x + 5;
}
```

## Functions
Shoo has first-class functions, which means that functions are treated as variables and can be passed as parameters, stored in variables, and returned from other functions. 
### Function Declarations
The keyword ```func``` is used for function variables. To declare a variable of type ```func```, you must include the keyword ```func```, followed by opening and closing parentheses. Inside the parentheses, you need two parts. First, you should put a list of the types of the parameters in your function followed by a semicolon. If your function doesn't have any parameters, you can simply put the semicolon. Second, you should put the complete return type of the function.
 
After the closing parentheses, remember to name your function as it is a variable and therefore needs a name, and remember to either terminate the variable declaration with a semicolon or define it using the assignment operator followed by the relevant expression.

```
/* Declare a variable named a to hold a function that takes an int as a parameter and returns a string. */
func(int; string) a;

/* Declare a variable named b to hold a function that takes an int and a float as parameters and returns a string. */
func(int, float; int) b;

/* Declare a variable named c to hold a function that has no parameters and returns void. */
func(; void) c;
```

### Function Definitions
The syntax for defining an anonymous function includes the keyword ```function```, parentheses containing zero or more arguments, each with a type and a name, followed by the return type and curly brackets ```{ }``` that contain the function body.
 
To define a named function, use the keyword ```function```, the function name, and then the parentheses containing zero or more arguments, each with a type and a name, followed by the return type and curly brackets ```{ }``` that contain the function body.
 
You can't assign a definition of a named function to a ```func``` variable.
 
As mentioned in the section "The Void Keyword," functions can also have return types of void. 

Mutually recursive functions are disallowed. 

```

// just defining a function does not require a terminating semicolon
function hello (string yourName) void {
  print("hello" + yourName);
}

// but a statement (here, assigning a function to a variable) does require a terminating semicolon
func(int, int; int) myFunc = 
function (int a, int b) int {
  return a + b;
};

func(; int) temp = function () int {
	return 5;
};

func(int; int) myMethod = function (int num) int {
  if (num == 0) {
    return 1;
  } else {
    return (temp() + num + 2);
  }
};

// The following does NOT work because the function on the right hand side is named.
func(int; int) myMethod2 = function a (int num) int {
  return num + 2;
};
```
 
The below is an example of a function with another function passed in as a parameter.
```
func(func(int;int), int; int) myFunc = function (func(int; int) f, int x) int {
	int result = f(x)+5;
	return result;
};
```
And because we have first-class functions, we can also define functions inside of other functions (and then return these functions and so on).
 
Here's a function that returns another function:
```
function makeAdd (int add) func(int;int) {
	return function (int value) int {
		return add + value;
	};
}

// make a function that adds 5 to the input
func(int;int) myAdd = makeAdd(5);

int x = 6;

println(str_of_int(myAdd(6))); // this prints 11
```

### Calling Functions
To call a function, put the function name followed by parentheses. Inside the parentheses, put the arguments for the function. The arguments you provide need to match the type and number of arguments that your function expects. Finally, put a semi-colon on the end this expression.

```
function add (int x, int y) int {
  return x + y;
}

add(5, 3); // function call to add() function.
```

## Built-in Functions
Built-in functions are functions that are predefined in the Shoo language. These functions can be called anywhere without the user having to define them first. All built-in functions are also first-class functions.
### print
The ```print()``` function can be used to print a string. It takes one argument: the string you want to print. This argument cannot be missing. 

### println
println is the same as ```print()```, except it also prints a newline after the string that is passed in. If you wish to just print a newline, you can achieve that by passing an empty string (```""```) to the function call. 

### str of bool
This function takes a boolean and returns a string version of the boolean value by printing true if the boolean is true and false if the boolean is false.

### str of float
This function takes a float and returns a string version of that float following the C language printing style achieved when using printf with ```%g``` in C.

### str of int
This function takes an integer and returns a string version of the digits in the integer.

### string concat
This function concatenates two strings. It is equivalent to using the ```+``` with strings as explained in section 6.3.1. Plus Operator.

### string equals
This function takes two strings and returns one if they are equal and zero if they are not equal. It tests semantic equality, not structural equality. You can also compare strings using the ```==``` operator, which gives the boolean true if the strings are equal and false otherwise. See section 3.6.4. Logical Operators for more information about the ```==``` operator.

### int of float
This function takes a float and returns an int by rounding the float to its nearest integer representation.

### int of str
This function takes a string and returns a integer by converting the characters in the string into their corresponding digits. If the string provided doesn't represent a valid base-10 integer, this function returns -1.
If the provided integer value is larger than the INT MAX defined by the C library, the behavior of this function is undefined. 

### float of int
This function takes an integer and returns the floating point representation of that integer.

### scan line
This function scans a newline terminated string from stdin and returns the string without the newline.

### scan char
This function scans a single character from stdin and returns it as a string. If the newline character is scanned, this function will return an empty string. If a character is scanned that can't be represented in Shoo's strings (i.e. escaped characters) this results in undefined behavior.

### die
This function takes a string as argument and an int, which represents an error code, and will terminate the program with the given error code and print the string before exiting.

### exit success
This function will terminate the program with no error code (as if a successful termination).

### rand autoseed
This function seeds a random number generator with the current time. Only call this function once per use of the random number generator. If you want to seed the random number generator, this needs to be called before rand afterseed is used.

### rand afterseed
This function randomly generates numbers. Call this function after calling rand autoseed if you want a more randomized seed than 0 (which this will default to if rand autoseed is not called first). This function will return an int which may be large. To control the size of the returned random number, you can mod the output by 100 or 1000 or 10 etc.

