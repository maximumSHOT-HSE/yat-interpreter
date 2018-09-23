import random
from model import *
from io import StringIO
import sys


class PrettyPrinter:

    def __init__(self):
        self.spaces = 0
        self.semicolon = 1

    def visit(self, tree):
        tree.accept(self)
        if self.semicolon:
            print(";", end='')

    def visitConditional(self, conditional):
        print(" " * self.spaces + "if (", end='')

        buff_spaces = self.spaces
        buff_semicolon = self.semicolon
        self.spaces = 0
        self.semicolon = 0

        self.visit(conditional.condition)

        self.spaces = buff_spaces
        self.semicolon = buff_semicolon

        print(") {")

        self.spaces += 4
        buff_semicolon = self.semicolon
        self.semicolon = 1

        for sent in conditional.if_true:
            self.visit(sent)
            print("")

        print(" " * (self.spaces - 4) + "} else {")

        for sent in conditional.if_false:
            self.visit(sent)
            print("")

        self.spaces -= 4
        self.semicolon = buff_semicolon

        print(" " * self.spaces + "}", end='')

    def visitNumber(self, number):
        print(" " * self.spaces + str(int(number.value)), end='')

    def visitFunctionDefinition(self, funcDef):
        print(" " * self.spaces + "def " + funcDef.name + "(", end='')

        anyArg = 0

        for arg in funcDef.function.args:
            if anyArg:
                print(", ", end='')
            else:
                anyArg = 1

            print(arg, end='')

        print(") {")

        self.spaces += 4
        buff_semicolon = self.semicolon
        self.semicolon = 1

        for statement in funcDef.function.body:
            self.visit(statement)
            print("")

        self.spaces -= 4
        self.semicolon = buff_semicolon

        print(" " * self.spaces + "}", end='')

    def visitPrint(self, printer):
        print(" " * self.spaces + "print ", end='')

        buff_spaces = self.spaces
        buff_semicolon = self.semicolon
        self.spaces = 0
        self.semicolon = 0

        self.visit(printer.expr)

        self.spaces = buff_spaces
        self.semicolon = buff_semicolon

    def visitRead(self, reader):
        print(" " * self.spaces + "read " + reader.name, end='')

    def visitReference(self, ref):
        print(" " * self.spaces + ref.name, end='')

    def visitBinaryOperation(self, binary):
        print(" " * self.spaces + "(", end='')

        buff_spaces = self.spaces
        buff_semicolon = self.semicolon
        self.spaces = 0
        self.semicolon = 0

        self.visit(binary.lhs)

        print(" " + binary.op + " ", end='')

        self.visit(binary.rhs)

        self.spaces = buff_spaces
        self.semicolon = buff_semicolon

        print(")", end='')

    def visitUnaryOperation(self, unary):
        print(" " * self.spaces + "(" + unary.op + "(", end='')

        buff_spaces = self.spaces
        buff_semicolon = self.semicolon
        self.spaces = 0
        self.semicolon = 0

        self.visit(unary.expr)

        self.spaces = buff_spaces
        self.semicolon = buff_semicolon

        print("))", end='')

    def visitFunctionCall(self, functionCall):
        print(" " * self.spaces, end='')

        buff_spaces = self.spaces
        buff_semicolon = self.semicolon
        self.spaces = 0
        self.semicolon = 0

        self.visit(functionCall.fun_expr)

        print("(", end='')

        anyArg = 0

        for arg in functionCall.args:
            if anyArg:
                print(", ", end='')
            else:
                anyArg = 1

            self.visit(arg)

        self.spaces = buff_spaces
        self.semicolon = buff_semicolon

        print(")", end='')


def testNumbers():

    memStdout = sys.stdout
    sys.stdout = StringIO()

    MAX_VALUE = 10**5

    x = Number(random.randint(-MAX_VALUE, MAX_VALUE))

    printer = PrettyPrinter()
    printer.visit(x)

    assert sys.stdout.getvalue() == str(x.value) + ";"

    sys.stdout = memStdout


def testBasicReference():

    memStdout = sys.stdout
    sys.stdout = StringIO()

    printer = PrettyPrinter()

    ref1 = Reference("foo")
    ref2 = Reference("bar")

    printer.visit(ref1)
    print("")
    printer.visit(ref2)

    assert sys.stdout.getvalue() == "foo;\nbar;"

    sys.stdout = memStdout


def testBasicPrint():

    memStdout = sys.stdout
    sys.stdout = StringIO()

    MAX_VLAUE = 20

    printer = PrettyPrinter()

    nm = Number(random.randint(-MAX_VLAUE, MAX_VLAUE))
    pr = Print(nm)

    printer.visit(pr)

    assert sys.stdout.getvalue() == "print " + str(nm.value) + ";"

    sys.stdout = memStdout


def testBasicRead():

    memStdout = sys.stdout
    sys.stdout = StringIO()

    printer = PrettyPrinter()

    printer.visit(Read("age"))
    print("")
    printer.visit(Read("current year"))

    assert sys.stdout.getvalue() == "read age;\nread current year;"

    sys.stdout = memStdout


def testBasicBinaryOperation():

    operations = \
        ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||"]

    MAX_VALUE = 20

    printer = PrettyPrinter()

    for op in operations:
        x = Number(random.randint(-MAX_VALUE, MAX_VALUE))
        y = Number(random.randint(-MAX_VALUE, MAX_VALUE))

        binary = BinaryOperation(x, op, y)

        memStdout = sys.stdout
        sys.stdout = StringIO()

        printer.visit(binary)

        assert \
            sys.stdout.getvalue() == \
            "(" + str(x.value) + " " + op + " " + str(y.value) + ");"

        sys.stdout = memStdout


def testBasicUnaryOperation():

    operations = ["!", "-"]

    MAX_VALUE = 20

    printer = PrettyPrinter()

    for op in operations:
        x = Number(random.randint(-MAX_VALUE, MAX_VALUE))

        unary = UnaryOperation(op, x)

        memStdout = sys.stdout
        sys.stdout = StringIO()

        printer.visit(unary)

        assert sys.stdout.getvalue() == "(" + op + "(" + str(x.value) + "));"

        sys.stdout = memStdout


def testBasicConditional():

    memStdout = sys.stdout
    sys.stdout = StringIO()

    printer = PrettyPrinter()

    x = Number(-17)
    y = Number(1)
    z = Number(2)

    cond = Conditional(
        BinaryOperation(x, "<", y),
        [
            Conditional(
                BinaryOperation(y, "<", z),
                [Print(UnaryOperation("-", z))],
                [Print(BinaryOperation(x, "*", y))]
            )
        ],
        [
            Conditional(
                BinaryOperation(x, "<", z),
                [Print(z)],
                [Print(Reference("Hello world!"))]
            )
        ]
    )

    printer.visit(cond)

    assert sys.stdout.getvalue() == "if ((-17 < 1)) {\n" + \
                                    "    if ((1 < 2)) {\n" + \
                                    "        print (-(2));\n" + \
                                    "    } else {\n" + \
                                    "        print (-17 * 1);\n" + \
                                    "    };\n" + \
                                    "} else {\n" + \
                                    "    if ((-17 < 2)) {\n" + \
                                    "        print 2;\n" + \
                                    "    } else {\n" + \
                                    "        print Hello world!;\n" + \
                                    "    };\n" + \
                                    "};"

    sys.stdout = StringIO()

    cond = Conditional(
        Number(0),
        [],
        []
    )

    printer.visit(cond)

    assert sys.stdout.getvalue() == "if (0) {\n" + \
                                    "} else {\n" + \
                                    "};"

    sys.stdout = memStdout


def testBasicFunctions():

    memStdout = sys.stdout

    printer = PrettyPrinter()

    fd_sum = FunctionDefinition("sum",
                                Function(("x", "y"), [
                                    BinaryOperation(
                                        Reference("x"),
                                        "+",
                                        Reference("y"))]))
    fd_mul = FunctionDefinition("mul",
                                Function(("x", "y"), [
                                    BinaryOperation(
                                        Reference("x"),
                                        "*",
                                        Reference("y"))]))

    sys.stdout = StringIO()

    printer.visit(fd_sum)

    assert sys.stdout.getvalue() == "def sum(x, y) {\n" + \
                                    "    (x + y);\n" + \
                                    "};"

    sys.stdout = StringIO()

    printer.visit(fd_mul)

    assert sys.stdout.getvalue() == "def mul(x, y) {\n" + \
                                    "    (x * y);\n" + \
                                    "};"

    sys.stdout = StringIO()

    printer.visit(FunctionCall(fd_sum, [Number(4), Number(13)]))

    assert sys.stdout.getvalue() == "def sum(x, y) {\n" + \
                                    "    (x + y);\n" + \
                                    "}(4, 13);"

    sys.stdout = StringIO()

    printer.visit(FunctionCall(fd_mul, [Number(-3), Number(7)]))

    assert sys.stdout.getvalue() == "def mul(x, y) {\n" + \
                                    "    (x * y);\n" + \
                                    "}(-3, 7);"

    sys.stdout = StringIO()

    printer.visit(FunctionCall(Reference("foo"), []))

    assert sys.stdout.getvalue() == "foo();"

    fd_mulsum = FunctionDefinition("mulSum", Function(("a", "b", "c", "d"), [
        fd_sum,
        fd_mul,
        FunctionCall(Reference("foo"), [Reference("bar1"), Reference("bar2")])
    ]))

    sys.stdout = StringIO()

    printer.visit(fd_mulsum)

    assert sys.stdout.getvalue() == "def mulSum(a, b, c, d) {\n" + \
                                    "    def sum(x, y) {\n" + \
                                    "        (x + y);\n" + \
                                    "    };\n" + \
                                    "    def mul(x, y) {\n" + \
                                    "        (x * y);\n" + \
                                    "    };\n" + \
                                    "    foo(bar1, bar2);\n" + \
                                    "};"

    sys.stdout = StringIO()

    printer.visit(FunctionDefinition("emptyFunction", Function((), [])))

    assert sys.stdout.getvalue() == "def emptyFunction() {\n" + \
                                    "};"

    sys.stdout = StringIO()

    printer.visit(FunctionCall(Reference("mulSum"), [
        Number(1), Number(2), Number(3), Number(4)]))

    assert sys.stdout.getvalue() == "mulSum(1, 2, 3, 4);"

    sys.stdout = memStdout


def testBigConditional():

    memStdout = sys.stdout

    printer = PrettyPrinter()

    cond = Conditional(
        Number(1),
        [
            Conditional(
                Number(1),
                [
                    Conditional(
                        Number(0),
                        [Print(Reference("smth"))],
                        [
                            FunctionDefinition("strange",
                                               Function(("a"), [
                                                   Print(Reference("a"))]))
                        ]
                    )
                ],
                [
                    FunctionCall(Reference("important"), [
                        Reference("x"), Reference("y"), Reference("z")]),
                    BinaryOperation(Reference("x"), "%", Reference("y"))
                ]
            )
        ],
        [
            UnaryOperation("!", Number(-12))
        ]
    )

    sys.stdout = StringIO()

    printer.visit(cond)

    assert sys.stdout.getvalue() == "if (1) {\n" + \
                                    "    if (1) {\n" + \
                                    "        if (0) {\n" + \
                                    "            print smth;\n" + \
                                    "        } else {\n" + \
                                    "            def strange(a) {\n" + \
                                    "                print a;\n" + \
                                    "            };\n" + \
                                    "        };\n" + \
                                    "    } else {\n" + \
                                    "        important(x, y, z);\n" + \
                                    "        (x % y);\n" + \
                                    "    };\n" + \
                                    "} else {\n" + \
                                    "    (!(-12));\n" + \
                                    "};"

    sys.stdout = memStdout


def testABS():

    memStdout = sys.stdout

    printer = PrettyPrinter()

    abs = FunctionDefinition("abs", Function(("x"), [
        Conditional(
            BinaryOperation(Reference("x"), "<", Number(0)),
            [UnaryOperation("-", Reference("x"))],
            [Reference("x")]
        )
    ]))

    sys.stdout = StringIO()

    printer.visit(abs)

    assert sys.stdout.getvalue() == "def abs(x) {\n" + \
                                    "    if ((x < 0)) {\n" + \
                                    "        (-(x));\n" + \
                                    "    } else {\n" + \
                                    "        x;\n" + \
                                    "    };\n" + \
                                    "};"

    sys.stdout = StringIO()

    printer.visit(FunctionCall(Reference("abs"), [Number(-4)]))

    assert sys.stdout.getvalue() == "abs(-4);"

    sys.stdout = StringIO()

    printer.visit(FunctionCall(Reference("abs"), [Number(17)]))

    assert sys.stdout.getvalue() == "abs(17);"

    sys.stdout = memStdout


def testFactorial():

    memStdout = sys.stdout

    printer = PrettyPrinter()

    fact_def = FunctionDefinition("fact", Function(("x"), [
        Conditional(
            BinaryOperation(Reference("x"), "<=", Number(1)),
            [Number(1)],
            [BinaryOperation(FunctionCall(
                Reference("fact"), [BinaryOperation(
                    Reference("x"),
                    "-",
                    Number(1))]),
                "*",
                Reference("x"))])
    ]))

    sys.stdout = StringIO()

    printer.visit(fact_def)

    assert sys.stdout.getvalue() == "def fact(x) {\n" + \
                                    "    if ((x <= 1)) {\n" + \
                                    "        1;\n" + \
                                    "    } else {\n" + \
                                    "        (fact((x - 1)) * x);\n" + \
                                    "    };\n" + \
                                    "};"

    sys.stdout = StringIO()

    printer.visit(FunctionCall(Reference("fact"), [Number(7)]))

    assert sys.stdout.getvalue() == "fact(7);"

    sys.stdout = memStdout


def testGCD():

    memStdout = sys.stdout

    printer = PrettyPrinter()

    gcd_def = FunctionDefinition("gcd", Function(("x", "y"), [
        Conditional(
            BinaryOperation(Reference("x"), "==", Number(0)),
            [Reference("y")],
            [FunctionCall(Reference("gcd"), [
                BinaryOperation(Reference("y"), "%", Reference("x")),
                Reference("x")
            ])]
        )
    ]))

    sys.stdout = StringIO()

    printer.visit(gcd_def)

    assert sys.stdout.getvalue() == "def gcd(x, y) {\n" + \
                                    "    if ((x == 0)) {\n" + \
                                    "        y;\n" + \
                                    "    } else {\n" + \
                                    "        gcd((y % x), x);\n" + \
                                    "    };\n" + \
                                    "};"

    sys.stdout = StringIO()

    printer.visit(FunctionCall(
        Reference("gcd"), (Reference("x"), Reference("y"))))

    assert sys.stdout.getvalue() == "gcd(x, y);"

    sys.stdout = memStdout


def testBigExamples():
    testBigConditional()
    testABS()
    testFactorial()
    testGCD()


def myTests():
    testNumbers()
    testBasicReference()
    testBasicPrint()
    testBasicRead()
    testBasicBinaryOperation()
    testBasicUnaryOperation()
    testBasicConditional()
    testBasicFunctions()
    testBigExamples()


if __name__ == "__main__":
    myTests()
