import math
import random
import numpy as np


class Scope:

    """Scope - представляет доступ к значениям по именам
    (к функциям и именованным константам).
    Scope может иметь родителя, и если поиск по имени
    в текущем Scope не успешен, то если у Scope есть родитель,
    то поиск делегируется родителю.
    Scope должен поддерживать dict-like интерфейс доступа
    (см. на специальные функции __getitem__ и __setitem__)
    """

    def __init__(self, parent=None):
        self.parent = parent
        self.nameDict = dict()

    def __setitem__(self, key, value):
        self.nameDict[key] = value

    def __getitem__(self, item):
        if item in self.nameDict:
            return self.nameDict[item]
        elif self.parent is not None:
            return self.parent[item]


class Number:

    """Number - представляет число в программе.
    Все числа в нашем языке целые."""

    def __init__(self, value):
        self.value = int(value)

    def evaluate(self, scope):
        return self

    def __eq__(self, other):
        return self.value == other.value

    def __ne__(self, other):
        return self.value != other.value

    def __lt__(self, other):
        return self.value < other.value

    def __gt__(self, other):
        return self.value > other.value

    def __le__(self, other):
        return self.value <= other.value

    def __ge__(self, other):
        return self.value >= other.value

    def __hash__(self):
        MOD = 2000003
        res = (self.value**2 + self.value) % MOD
        return (res % MOD + MOD) % MOD

    def __add__(self, other):
        return Number(self.value + other.value)

    def __sub__(self, other):
        return Number(self.value - other.value)

    def __mul__(self, other):
        return Number(self.value * other.value)

    def __floordiv__(self, other):
        return Number(self.value // other.value)

    def __mod__(self, other):
        return Number(self.value % other.value)

    def __neg__(self):
        return Number(-self.value)

    def accept(self, visitor):
        return visitor.visitNumber(self)


class Function:

    """Function - представляет функцию в программе.
    Функция - второй тип поддерживаемый языком.
    Функции можно передавать в другие функции,
    и возвращать из функций.
    Функция состоит из тела и списка имен аргументов.
    Тело функции это список выражений,
    т. е.  у каждого из них есть метод evaluate.
    Список имен аргументов - список имен
    формальных параметров функции.
    Аналогично Number, метод evaluate должен возвращать self.
    """

    def __init__(self, args, body):
        self.args = args
        self.body = body

    def evaluate(self, scope):
        return self

    def accept(self, visitor):
        return self


class FunctionDefinition:

    """FunctionDefinition - представляет определение функции,
    т. е. связывает некоторое имя с объектом Function.
    Результатом вычисления FunctionDefinition является
    обновление текущего Scope - в него
    добавляется новое значение типа Function."""

    def __init__(self, name, function):
        self.name = name
        self.function = function

    def evaluate(self, scope):
        scope[self.name] = self.function
        return self.function

    def accept(self, visitor):
        return visitor.visitFunctionDefinition(self)


class Conditional:

    """
    Conditional - представляет ветвление в программе, т. е. if.
    """

    def __init__(self, condtion, if_true, if_false=None):
        self.condition = condtion
        self.if_true = if_true
        self.if_false = if_false

    def evaluate(self, scope):
        res = None
        if self.condition.evaluate(scope).value == 0:
            for expr in self.if_false:
                res = expr.evaluate(scope)
        else:
            for expr in self.if_true:
                res = expr.evaluate(scope)
        return res

    def accept(self, visitor):
        return visitor.visitConditional(self)


class Print:

    """Print - печатает значение выражения на отдельной строке."""

    def __init__(self, expr):
        self.expr = expr

    def evaluate(self, scope):
        res = self.expr.evaluate(scope)
        print(res.value)
        return res

    def accept(self, visitor):
        return visitor.visitPrint(self)


class Read:

    """Read - читает число из стандартного потока ввода
     и обновляет текущий Scope.
     Каждое входное число располагается на отдельной строке
     (никаких пустых строк и лишних символов не будет).
     """

    def __init__(self, name):
        self.name = name

    def evaluate(self, scope):
        value = int(input())
        scope[self.name] = Number(value)
        return Number(value)

    def accept(self, visitor):
        return visitor.visitRead(self)


class FunctionCall:

    """
    FunctionCall - представляет вызов функции в программе.
    В результате вызова функции должен создаваться новый объект Scope,
    являющий дочерним для текущего Scope
    (т. е. текущий Scope должен стать для него родителем).
    Новый Scope станет текущим Scope-ом при вычислении тела функции.
    """

    def __init__(self, fun_expr, args):
        self.fun_expr = fun_expr
        self.args = args

    def evaluate(self, scope):
        function = self.fun_expr.evaluate(scope)
        call_scope = Scope(scope)

        for argName, arg in zip(function.args, self.args):
            call_scope[argName] = arg.evaluate(scope)

        res = None
        for expr in function.body:
            res = expr.evaluate(call_scope)

        return res

    def accept(self, visitor):
        return visitor.visitFunctionCall(self)


class Reference:

    """Reference - получение объекта
    (функции или переменной) по его имени."""

    def __init__(self, name):
        self.name = name

    def evaluate(self, scope):
        return scope[self.name]

    def accept(self, visitor):
        return visitor.visitReference(self)


class BinaryOperation:

    """BinaryOperation - представляет бинарную операцию над двумя выражениями.
    Результатом вычисления бинарной операции является объект Number.
    Поддерживаемые операции:
    “+”, “-”, “*”, “/”, “%”, “==”, “!=”,
    “<”, “>”, “<=”, “>=”, “&&”, “||”."""

    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

    def evaluate(self, scope):
        a = self.lhs.evaluate(scope)
        b = self.rhs.evaluate(scope)
        if self.op == "+":
            return a + b
        elif self.op == "-":
            return a - b
        elif self.op == "*":
            return a * b
        elif self.op == "/":
            return a // b
        elif self.op == "%":
            return a % b
        elif self.op == "==":
            return Number(a == b)
        elif self.op == "!=":
            return Number(a != b)
        elif self.op == "<":
            return Number(a < b)
        elif self.op == ">":
            return Number(a > b)
        elif self.op == "<=":
            return Number(a <= b)
        elif self.op == ">=":
            return Number(a >= b)
        elif self.op == "&&":
            return Number(a.value != 0 and b.value != 0)
        elif self.op == "||":
            return Number(a.value != 0 or b.value != 0)

    def accept(self, visitor):
        return visitor.visitBinaryOperation(self)


class UnaryOperation:

    """UnaryOperation - представляет унарную операцию над выражением.
    Результатом вычисления унарной операции является объект Number.
    Поддерживаемые операции: “-”, “!”."""

    def __init__(self, op, expr):
        self.op = op
        self.expr = expr

    def evaluate(self, scope):
        x = self.expr.evaluate(scope)
        if self.op == "-":
            return -x
        elif self.op == "!":
            return Number(x.value == 0)

    def accept(self, visitor):
        return visitor.visitUnaryOperation(self)


def example():
    parent = Scope()
    parent["foo"] = Function(('hello', 'world'),
                             [Print(BinaryOperation(Reference('hello'),
                                                    '+',
                                                    Reference('world')))])
    parent["bar"] = Number(10)
    scope = Scope(parent)
    assert 10 == scope["bar"].value
    scope["bar"] = Number(20)
    assert scope["bar"].value == 20
    print('It should print 2: ', end=' ')
    printer = Print(FunctionCall(FunctionDefinition('foo', parent['foo']),
                                 [Number(5), UnaryOperation('-', Number(3))]))

    printer.evaluate(scope)


def testNumbers():

    mainScope = Scope()

    mainScope["a"] = Number(13)

    assert mainScope["a"].value == 13

    sonScope1 = Scope(mainScope)
    sonScope2 = Scope(mainScope)

    sonScope1["b"] = Number(7)
    sonScope2["b"] = Number(9)

    assert sonScope1["b"].value == 7
    assert sonScope2["b"].value == 9

    sonScope2["a"] = Number(19)

    assert sonScope2["a"].value == 19
    assert mainScope["a"].value == 13


def testBinaryOperations():

    # print("test Binary Operations: ", end='')

    # Numbers already tested

    operations = \
        ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||"]

    TEST_NUMBER = 100
    MAX_VALUE = 10**9

    scope = Scope()

    for op in operations:

        for _ in range(TEST_NUMBER):
            x = random.randint(-MAX_VALUE, MAX_VALUE)
            y = random.randint(-MAX_VALUE, MAX_VALUE)

            if op in ["/", "%"]:
                while y == 0:
                    y = random.randint(-MAX_VALUE, MAX_VALUE)

            shouldBe = int(eval((str(x) + op + str(y)).
                                replace("/", "//").
                                replace("&&", " and ").
                                replace("||", " or ")))

            if op in ["&&", "||"]:
                shouldBe = int(bool(shouldBe))

            butHave = BinaryOperation(
                Number(x), op, Number(y)).evaluate(scope).value

            assert shouldBe == butHave

    # print("OK")


def testUnaryOperations():

    # print("test Unary Operations: ", end='')

    # Numbers already tested

    operations = ["-", "!"]

    TEST_NUMBER = 100
    MAX_VALUE = 10**9

    scope = Scope()

    for op in operations:

        for _ in range(TEST_NUMBER):

            x = random.randint(-MAX_VALUE, MAX_VALUE)

            if op == "!":
                x = random.randint(0, 1)

            shouldBe = \
                int(eval((op + "(" + str(x) + ")").replace("!", "not ")))

            butHave = UnaryOperation(
                op, Number(x)).evaluate(scope).value

            assert shouldBe == butHave

            # print("OK")


def testFunctionMulSum():
    scope = Scope()

    FunctionDefinition("sum",
                       Function(("x", "y"), [
                           BinaryOperation(
                               Reference("x"),
                               "+",
                               Reference("y"))])).evaluate(scope)

    FunctionDefinition("mul",
                       Function(("x", "y"), [
                           BinaryOperation(
                               Reference("x"),
                               "*",
                               Reference("y"))])).evaluate(scope)

    TEST_NUMBER = 100
    MAX_VALUE = 10**9

    for _ in range(TEST_NUMBER):
        x = random.randint(-MAX_VALUE, MAX_VALUE)
        y = random.randint(-MAX_VALUE, MAX_VALUE)

        assert x + y == FunctionCall(scope["sum"], [
            Number(x), Number(y)]).evaluate(scope).value

        assert x * y == FunctionCall(scope["mul"], [
            Number(x), Number(y)]).evaluate(scope).value

    FunctionDefinition("mulSum",
                       Function(
                           ("a", "b", "c", "d"), [
                               FunctionCall(
                                   scope["mul"], [
                                       FunctionCall(
                                           scope["sum"], [
                                               Reference("a"),
                                               Reference("b")]),
                                       FunctionCall(
                                           scope["sum"], [
                                               Reference("c"),
                                               Reference("d")])])])).\
        evaluate(scope)

    for _ in range(TEST_NUMBER):
        a = random.randint(-MAX_VALUE, MAX_VALUE)
        b = random.randint(-MAX_VALUE, MAX_VALUE)
        c = random.randint(-MAX_VALUE, MAX_VALUE)
        d = random.randint(-MAX_VALUE, MAX_VALUE)

        assert (a + b) * (c + d) == FunctionCall(scope["mulSum"], [
            Number(a), Number(b), Number(c), Number(d)]).evaluate(scope).value


def testFunctionsInDifferentScopes():

    mainScope = Scope()

    FunctionDefinition("strange",
                       Function(("x", "y"), [
                           BinaryOperation(
                               Reference("x"),
                               "+",
                               Reference("y"))])).evaluate(mainScope)

    TEST_NUMBER = 100
    MAX_VALUE = 10**9

    for _ in range(TEST_NUMBER):
        x = random.randint(-MAX_VALUE, MAX_VALUE)
        y = random.randint(-MAX_VALUE, MAX_VALUE)

        assert x + y == FunctionCall(
            mainScope["strange"], [
                Number(x), Number(y)]).evaluate(mainScope).value

    sonScope1 = Scope(mainScope)
    sonScope2 = Scope(mainScope)

    FunctionDefinition("strange",
                       Function(("x", "y"), [
                           BinaryOperation(
                               Reference("x"),
                               "-",
                               Reference("y"))])).evaluate(sonScope1)

    FunctionDefinition("otherStrange",
                       Function(
                           ("x", "y"), [
                               UnaryOperation("!",
                                              BinaryOperation(
                                                  Reference("x"),
                                                  "<",
                                                  Reference("y")))])).\
        evaluate(sonScope2)

    for _ in range(TEST_NUMBER):
        x = random.randint(-MAX_VALUE, MAX_VALUE)
        y = random.randint(-MAX_VALUE, MAX_VALUE)

        assert x - y == FunctionCall(
            sonScope1["strange"], [
                Number(x), Number(y)]).evaluate(sonScope1).value

        assert x + y == FunctionCall(
            sonScope2["strange"], [
                Number(x), Number(y)]).evaluate(sonScope2).value


def testBasicFunctions():

    # Numbers, Binary, Unary already tested

    testFunctionMulSum()
    testFunctionsInDifferentScopes()


def testConditionAbs():

    scope = Scope()

    TEST_NUMBER = 100
    MAX_VALUE = 10 ** 9

    scope = Scope()

    for _ in range(TEST_NUMBER):
        x = random.randint(-MAX_VALUE, MAX_VALUE)

        shouldBe = int(math.fabs(x))

        butHave = Conditional(
            BinaryOperation(Number(x), "<", Number(0)),
            [UnaryOperation("-", Number(x))],
            [Number(x)]).evaluate(scope).value

        assert shouldBe == butHave


def testConditionMax():

    TEST_NUMBER = 100
    MAX_VALUE = 10**9

    scope = Scope()

    for _ in range(TEST_NUMBER):
        x = random.randint(-MAX_VALUE, MAX_VALUE)
        y = random.randint(-MAX_VALUE, MAX_VALUE)

        assert max(x, y) == Conditional(
            BinaryOperation(
                Number(x),
                "<",
                Number(y)),
            [Number(y)],
            [Number(x)]).evaluate(scope).value


def testConditional():

    # Numbers, BasicFunctions, Unary and binary operations already tested

    testConditionAbs()
    testConditionMax()


def testRecursionFactorial():

    scope = Scope()

    FunctionDefinition("fact", Function(("x"), [
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
    ])).evaluate(scope)

    TEST_NUMBER = 100
    MAX_VALUE = 50

    for _ in range(TEST_NUMBER):
        x = random.randint(0, MAX_VALUE)

        assert math.factorial(x) == FunctionCall(
            scope["fact"], [Number(x)]).evaluate(scope).value


def testRecursionGCD():

    scope = Scope()

    FunctionDefinition("gcd", Function(("x", "y"), [
        Conditional(
            BinaryOperation(Reference("x"), "==", Number(0)),
            [Reference("y")],
            [FunctionCall(Reference("gcd"), [
                BinaryOperation(Reference("y"), "%", Reference("x")),
                Reference("x")
            ])]
        )
    ])).evaluate(scope)

    TEST_NUMBER = 100
    MAX_VALUE = 10**9

    for _ in range(TEST_NUMBER):
        k = random.randint(1, MAX_VALUE)
        x = random.randint(1, MAX_VALUE // k) * k
        y = random.randint(1, MAX_VALUE // k) * k

        assert math.gcd(x, y) == FunctionCall(
            scope["gcd"], [Number(x), Number(y)]).evaluate(scope).value


def testRecursion():

    testRecursionFactorial()
    testRecursionGCD()


def testPrintRead():

    MAX_N = 1000
    MAX_VALUE = 10**9

    scope = Scope()

    n = random.randint(1, MAX_N)

    a = np.random.randint(-MAX_VALUE, MAX_VALUE, n)

    Print(Number(n)).evaluate(scope)

    for x in a:
        Print(Number(x)).evaluate(scope)

    Read("m").evaluate(scope)

    assert scope["m"].value == n

    for i in range(n):
        Read("x").evaluate(scope)
        assert scope["x"].value == a[i]


def myTests():
    testNumbers()
    testBinaryOperations()
    testUnaryOperations()
    testBasicFunctions()
    testConditional()
    testRecursion()
    testPrintRead()

if __name__ == '__main__':
    # example()
    myTests()
