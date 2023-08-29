---
"title":"How To Build Your Own Programming Language"
"date":"Jan 2, 2021"
---

Building your own programming language is a really fun project. It's also not as hard as it sounds. Through this post, I want to walk you through the steps you can take to start your journey in programming language development.

I studied a lot of great resources before I was able to build my first language. I noticed that a lot of these resources began by implementing really big languages. This resulted in me losing patience and giving up countless times since it took far too long to see any actual results.

A lot of resources also give away the code at the beginning and then give a line by line description of what's going on. While this approach works for many, I personally find it more fun when I just have a blueprint of how to implement something and then actually figure out the code myself.

This post takes a different approach in trying to teach how to get started with programming language development. We will see how to implement an extremely tiny language. One that does not support more than a couple features. I'll explain each step in detail and for each step, I'll provide minimal pseudocode. This way, you'll have a clear understanding on what you have to do to create your own language but you'll still have to figure out the code yourself. 

By going the process of figuring out the algorithms, fixing bugs and googling when you are stuck, you'll have a far more enriched learning experience than you would have if you had everything laid out for you. 

Since this post shows how to implement an extremely minimal language, you'll also have code running a lot faster.

### So, how does one write their own language?

Programs written in ANY language are really just files with text on them. When you write python code on a .py file, you are really just writing text on a file. To the computer, there is no absolutely no difference than when you are writing, let's say an English essay. We need another program that can take that file of code, decipher its meaning, and tell the CPU what it is trying to do. I think you can predict what we have to do then. In order to build our own  language, we have to build one of these programs that can understand our language. 

### What are these programs? 

These programs are compilers and interpreters. You can build either to implement your language, they each have their pros and cons. I'll explain both but for this post, we will mostly be focusing on interpreters. It is important to note that by the end of this read, you'll start to see how the two are not so different after all and once you build one, you'll develop a natural intuition on how to build the other.

### Compilers vs Interpreters

Compilers take programs written in one language as input and then output a translation of that same program in another language. How does that help? Most compilers translate our program into Assembly language. Once you have the assembly code, you can simply get an *assembler* to translate the file to binary which the CPU can directly execute.  

Interpreters are simpler. Instead of outputting a translation of your code, interpreters directly execute your code. 

Compilers and interpreters are extremely similar in the sense that they both have to make sense of the code you are feeding it. In fact, you can use the exact same code you will write for this interpreter to make it understand the syntax of your language in your compiler. 

So, let's go and see what we have to do in order to build an interpreter. 


### Step 1 : Scanning 

The first step towards building an interpreter or compiler is to build the scanner. The scanner takes in raw source code as input and produces a sequence of meaningful pieces called tokens. You can think of a token as a data type in your interpreter that has 2 fields : a type and a lexeme.

--code

    class Token {
        string type
        string lexeme
    }
--endcode
A lexeme is a sequence of characters that hold some meaning in our language. Think of the following line of code:

--code
    var num = 10;
--endcode
From this line, we can extract 5 lexemes : "var", "num", "=", "10", and ";". These are all groups of characters that carry some meaning. 

The type field in the Token data type represents what the lexeme means in our language. For the above example, the lexeme "var" is of type "VAR_KEYWORD" as it is a keyword in our language. The lexeme "num" is of type "IDENTIFIER". The lexeme "10" is of type "NUMBER" .The lexeme "=" is of type "EQUALS" and the lexeme ";" is of type "SEMI_COLON". We use strings to represent this field. While they are not as efficient as an int or byte, they are perfect for our purposes.

For the above code, we would get the following Tokens: {type: "KEYWORD", lexeme:"var"}, {type:"IDENTIFIER", lexeme:"num"}, {type:"EQUALS", lexeme:"="}, {type:"NUMBER",lexeme: "10"},{type:"SEMI_COLON", lexeme:";"}

Now that we know what Tokens are, we can understand what a scanner does. A scanner goes through the raw source code and produces an array of token objects.
--code
    class Scanner {
        scan(string source) []Token {

        }
    }
--endcode
See if you can predict what the scanner will output for the following block of code :
--code
    if (15 > 5) {
        print("hello");
    }
--endcode
The scanner would produce the following array:
--code
    [{type: "KEYWORD", lexeme :"if"}, 
    {type: "LEFT_BRACKET", lexeme: "("},
    {type: "NUMBER", lexeme: "15"},
    {type: "GREATER_THAN_SYMBOL", lexeme: ">"},
    {type: "NUMBER", lexeme: "5"},
    {type: "RIGHT_BRACKET", lexeme: ")"},
    {type: "OPENING_PARENTHESIS", lexeme: "{"},
    {type: "KEYWORD", lexeme: "print"},
    {type: "LEFT_BRACKET", lexeme: "("},
    {type: "STRING", lexeme: "hello"},
    {type: "RIGHT_BRACKET", lexeme: ")"},
    {type: "SEMI_COLON", lexeme: ";"},
    {type: "CLOSING_PARENTHESIS", lexeme: "}"}]
--endcode

Few things you might have noticed from our example : 

1. There's newline characters in the original source code but our array of tokens don't have them. This is because the newline character does not have any meaning in our language. They are not invalid but they also don't make a difference, so we simply ignore them. We only care about the tokens that are valid and meaningful. This is also purely a choice. In your language, you can have any character mean anything you want it to. 

2. The token in index 9 has "STRING" as the value for the field 'type' and "hello" as the value for the field 'lexeme.' When creating the lexeme, we got rid of the quotations in the raw source code. Instead of the lexeme being "\\"hello\\"", we got it to be "hello". This is because quotations only help the scanner identify that the current lexeme is of type "STRING." After the lexeme has been identified, we no longer need the quotations.

Now you might be wondering why we go through this process of scanning the source code. I had the same question when I first learned about it. The answer is that it makes building the other parts of our interpreter a lot simpler. When we convert the program from a giant string into a series of tokens, we make it easier for the interpreter to access specific parts of the code at runtime. If this does not make sense right away, don't worry, it will when we build our 'Parser' and 'Evaluator'.

By now, you are definitely ready to start building your interpreter by creating a scanner that takes in source code and produces a series of tokens. I have not given much details about how to actually implement a scanner but here's a few questions you should ponder that will help you get started with writing the code : 

1. Think about all the single character lexemes in your language. In many languages, these include "(", ")", "<", ">". When our scanner encounters one of these characters, what can it do so that we get a Token object representing the character?

2. Now, think about all the two character lexemes in your language. These can be things such as "!=", "==", ">=". How can we get our scanner to recognize these two character lexemes in the source code? If we encounter a "<", how can we determine whether the Token we are going to produce is going to be of type "LESS_THAN_EQUAL_SYMBOL" which is a two character lexeme, or of type "LESS_THAN_SYMBOL" which is a single character lexeme?

3. When we write programs, what do we usually define by using quotations? What should our scanner do when it encounters an opening quotation?

4. What should our scanner do when it encounters a digit?

5. What should our scanner do when it encounters a letter?


### Step 2 : Parsing

The second step is to build a parser. A parser is a tool that takes in some form of input and converts it into a data structure that makes the information in the input easier to access and manipulate. 

Consider the following string:
--code
    var str = "{\"key1\":\"foo\", \"key2\":\"bar\"}"
--endcode
This string is simply a string representation of a dictionary object. 

Now think about what you would need to do in order to get the value for the key 'key1.' 

One option would be to write an algorithm that iterates through the string, searches for the key and then returns the value following the semi colon. You can imagine how complicated this can get.  

An easier option would be to convert the string into an in-memory data structure that will allow us to access keys in a significantly simpler way. In Javascript, this can be a one-liner as simple as:
--code
    var json = JSON.parse(str)
--endcode
Now we can access any key we want simply by using dot notation. 
--code
    val = json.key1
    // val = "foo"
--endcode
We can also perform other operations such as replacing the value of keys. 
--code
    json.key2 = "hello"
--endcode
This is exactly what parsing is. Instead of working directly with the string, we converted it into a data structure that allowed us to access and manipulate the information embedded in the string in a simpler way.

We are now going to build a parser for our interpreter that will take in the array of inputs generated by the scanner and convert it into this data structure called an Abstract Syntax Tree.
--code
    class Parser {
        parse([]Token tokens)AbstractSyntaxTree {

        }
    }
--endcode
### Abstract Syntax Trees

You might not have noticed this before, but the syntax of programming languages are naturally recursive. Consider the following line of code:
--code
    var x = (10 + 2) + 4
--endcode
This is a declaration statement. After our interpreter evaluates it, the value of the variable x will equal 16. Let's see what our interpreter would need to do in order to actually evaluate the right side of the statement. 

The interpreter sees that it is an addition expression where the left side is (10 + 2) and the right side is 4. The interpreter will need to evaluate each side individually. 

First, it tries to evaluate the left side. It sees that it is an addition expression in which the left side is 10 and the right side is 2. In order to get the result of this addition expression, it will need to evaluate 10 and 2 individually. Evaluating 10 gives us 10 and evaluating 2 gives us 2. Now that the interpreter has the value for both sides, it tries to add them. After adding 10 with 2, the interpreter determines 12 to be the value of the left side. 

Then, the interpreter tries to evaluate the right side. It sees that the right side only contains a Number Expression. Evaluating 4 results in 4 and the interpreter determines that to be the result of the right side. 

The interpreter adds the value from the left side to the value from the right side. This results in 16.

Do you see the recursive structure? In order to evaluate an addition expression, we had to evaluate another addition expression. 

Due to this recursive nature of the syntax, a tree data structure becomes a natural choice for representing our source code. We use a special kind of tree called an Abstract Syntax Tree.

You can think of an abstract syntax tree as an n-ary tree in which each child can be evaluated. The children may have children of its own that can be evaluated. 

Before we give the data type definition of an abstract syntax in our interpreter, let's define an interface called 'Evaluatable.' Yes, I know it's not a real word  but it nicely describes the characteristic of all the classes that implement this interface. The 'Evaluatable' interface only has one method called Evaluate. This means that all implementations of this interface can be evaluated. 
--code
    interface Evaluatable  {
        Evaluate() Object {

        }
    }
--endcode
You will notice that the Evaluate method can return any type. This is because different types of evaluations will return different types of values. A boolean expression will return a boolean value, a mathematical expression will return an int or a float, and a string expression will return a string. 

Now that we have this interface, we can define the Abstract Syntax Tree data type in our interpreter.
--code
    class AbstractSyntaxTree {
        []Evaluatable children
    }
--endcode
### What ASTs are made of

The abstract syntax tree data type has a single field which is an array of Evaluatable expressions. We will see some examples of the type of elements that this array can consist of.

Let's start off with something simple. What would happen when our interpreter tries to evaluate a single number? We return the number itself. We can create a class of type Evaluatable to represent expressions of such type. 
--code
    class NumberExpression implements Evaluatable {
        Token numberNode

        Evalutate()Object {
            return int(this.numberNode.lexeme)
        }
    }
--endcode
Evaluating strings in our interpreter are the same as evaluating numbers. We can create a class of type Evalutable to evaluate strings. 
--code
    class StringExpression implements Evaluatable {
        Token strNode 

        Evalutate()Object {
            return string(this.strNode.lexeme)
        }

    }
--endcode
Instances of StringExpression and NumberExpression can be elements of the children field in the Abstract Syntax Tree data type. 

One thing you might have noticed is that the two examples we have seen so far are non recursive. If the abstract syntax tree has a child that is of type StringExpression, we know that child will not have any children. In order words, instances of StringExpression and NumberExpression are leaves in our abstract syntax tree. 

Let's look at some expression types that are recursive. Consider the following expression:
--code
    (5+10) > 312
--endcode
This is an expression that checks whether the value on the left side is greater than the right side. The expression will evaluate to a Boolean value. 

This is an example of a recursive expression in our interpreter. In order to get the final value, we need to first evaluate the expression on the left hand side and then evaluate the expression on the right hand side. Only after evaluating both child expressions can we get the final value. We can create a class to represent expressions of this form.
--code
    class GreaterThanExpression implements Evaluatable {
        Evaluatable leftExpression
        Evaluatable rightExpression

        Evaluate() Object {
            try {
                return this.leftExpression.evaluate() > this.rightExpression.evaluate()
            } catch { 
                // error as the value returned by the children expressions are not comparable.
            }
        }
    } 
--endcode

GreaterThanExpression is a class that implements the Evaluatable interface which means that instances of this class can be elements of the children field in the abstract syntax tree data type. The class has two fields which are both of type Evaluatable. These two fields can be thought of as the children as they can also be evaluated. 

### Recursive Descent Parsing

Now that we have a clear picture of what abstract syntax trees are made of, we are going to see how we can build one from the array of tokens we generated in the previous section. 

There's several parsing strategies out there but we're going to use one called Recursive Descent. Recursive Descent is a top down parsing strategy which means it builds the abstract syntax tree from top to bottom.

Consider the following line of code:
--code
    var str = "hello world"
--endcode
The scanner converts the raw source code into the following array of tokens.
--code
    [ 
        {type: "VAR_KEYWORD", lexeme="var"},
        {type: "IDENTIFIER", lexeme="str"},
        {type: "EQUALS", lexeme="="},
        {type:"STRING", lexeme="hello world"}
    ]
--endcode
We're going to see how this array can be parsed into an abstract syntax tree. First, we need a class that implements the Evalutable interface and represents declaration statements. 
--code
    class DeclarationStatement implements Evalutable {
        string identifier
        Evalutable rightSideExpression
        Dictionary<string, string>variables
        constructor(string identifier, Evalutable rightSideExpression, Dictionary<string, string>variables) { 
            this.identifier = identifier
            this.rightSideExpression = rightSideExpression
            this.variables = variables
        }

        Evaluate()Object {
            this.variables[this.identifier] = this.rightSideExpression.Evaluate()
        }
    }
--endcode
We also need a way to evaluate string expressions. 
--code
    class StringExpression {
        Token stringToken
        constructor(Token stringToken) {
            this.stringToken = stringToken
        }

        Evaluate()Object {
            return this.stringToken.lexeme
        }
    }
--endcode
Now that we have classes for evaluating both declaration statements and strings, we can write the code for the parser.  
--code
    class Parser {
        int currentPosition
        Dictionary<string, string> variables // a dictionary for storing all variables declared in our programs.
        parse([]Token tokens)AbstractSyntaxTree {
            children = []
            while this.currentPosition < length(tokens) {
                children.append(parseCurrentToken(tokens))
            }
            
            return new AbstractSyntaxTree(children)
        }

        private parseCurrentToken([]Token tokens) Evaluatable{
            if this.currentPosition < length(tokens) {
                if tokens[currentPosition].type == "VAR_KEYWORD" { 
                    return this.parseVarKeyword(tokens)
                }

                else if tokens[this.currentPosition].type == "STRING" {
                    return new StringExpression(tokens[this.currentPosition++])
                }
            } else {
                throw error
            }
            
        }

        private parseVarKeyword([]Token tokens) Evaluatable {
            // we know tokens[this.currentPosition] is a token of type var keyword.
            this.currentPosition += 1 
            this.checkIfCurTokenIsCorrect(tokens, "IDENTIFIER")
            identifier = tokens[this.currentPosition].lexeme
            this.currentPosition += 1
            this.checkIfCurTOkenIsCorrect(tokens, "EQUALS")
            this.currentPosition += 1
            var expr = this.parseCurrentToken(tokens)

            return new DeclarationStatement(identifier, expr, this.variables)
        }

        private checkIfCurTokenIsCorrect([]Token tokens, string expectedType) {
            if this.currentPosition < len(tokens) {
                if tokens[this.currentPosition].type != expectedType {
                    throw error
                } 
            } else {
                throw error
            }
        }
    }
--endcode
A brief description of what's going on in the code : The parser goes through each token in the array and checks its type. If the current token is of type "VAR_KEYWORD", the parser tries to create an instance of the class DeclarationStatement. In order to do that, the parser needs to see if all the required elements are there. These include an identifier, an equals sign, and a right hand expression. The parser recursively parses the right hand expression. Once we have the instance of DeclarationStatement with all the elements, the instance is returned and appended to the children array.

### Step 3 : Evaluating

This is the final step of building an interpreter. It's also the easiest. We have the source code parsed into an Abstract Syntax Tree and all that's left for us do is go through the tree and execute each section.

Since all children of our abstract syntax tree are of type Evaluatable, all we have to do is iterate through the list of children and call evaluate on each child. 
--code
    class Evaluator { 
        Evaluate(AbstractSyntaxTree ast){ 
            for child in ast.children { 
                child.Evaluate()
            }
        }
    }
--endcode
And, that's it! If you've defined all the classes that implement the Evaluatable interface correctly, this one call to Evaluate in each parent statement/expression will ensure that all the subexpressions are correctly evaluated. 

### Conclusion

The goal of this post was to get you started with programming language development by building an interpreter for an extremely tiny language. One that only supports a couple features.

If you were able to understand what the scanner, parser and evaluator do in an interpreter, you're ready to start reading more complex text and building bigger languages. 

I highly recommend the following resources:

1. [Crafting Interpreters by Bob Nystrom](https://craftinginterpreters.com/)
2. [Writing An Interpreter in Go by Thorston Ball](https://interpreterbook.com/)
3. [So you want to write an interpreter? A Talk by Alex Gaynor](https://www.youtube.com/watch?v=LCslqgM48D4&ab_channel=NextDayVideo)
