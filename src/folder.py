import random
from model import *
from printer import *


class ConstantFolder:

    def visit(self, tree):
        return tree.accept(self)

    def visitConditional(self, conditional):
        conditional.condition = self.visit(conditional.condition)

        for i in range(len(conditional.if_true)):
            conditional.if_true[i] = \
                self.visit(conditional.if_true[i])

        for i in range(len(conditional.if_false)):
            conditional.if_false[i] = \
                self.visit(conditional.if_false[i])

        return conditional

    def visitNumber(self, number):
        return number

    def visitFunctionDefinition(self, funcDef):
        for i in range(len(funcDef.function.body)):
            funcDef.function.body[i] = \
                self.visit(funcDef.function.body[i])
        return funcDef

    def visitPrint(self, printer):
        printer.expr = self.visit(printer.expr)
        return printer

    def visitRead(self, reader):
        return reader

    def visitReference(self, ref):
        return ref

    def visitBinaryOperation(self, binary):

        binary.lhs = self.visit(binary.lhs)
        binary.rhs = self.visit(binary.rhs)

        if isinstance(binary.lhs, Number) and \
                isinstance(binary.rhs, Number):
            return binary.evaluate(Scope())

        if binary.op == "*" and \
                isinstance(binary.lhs, Number) and \
                        binary.lhs == Number(0) and \
                isinstance(binary.rhs, Reference):
            return Number(0)

        if binary.op == "*" and \
                isinstance(binary.rhs, Number) and \
                        binary.rhs == Number(0) and \
                isinstance(binary.lhs, Reference):
            return Number(0)

        if binary.op == "-" and \
                isinstance(binary.lhs, Reference) and \
                isinstance(binary.rhs, Reference) and \
                        binary.lhs.name == binary.rhs.name:
            return Number(0)

        return binary

    def visitUnaryOperation(self, unary):

        unary.expr = self.visit(unary.expr)

        if isinstance(unary.expr, Number):
            return unary.evaluate(Scope())

        return unary

    def visitFunctionCall(self, functionCall, spaces):
        functionCall.fun_expr = self.visit(functionCall.fun_expr)

        for i in range(len(functionCall.args)):
            functionCall.args[i] = self.visit(functionCall.args[i])

        return functionCall


def testTwoNumbers():

    folder = ConstantFolder()

    operations = \
        ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||"]

    for op in operations:
        a = Number(random.randint(1, 20))
        b = Number(random.randint(1, 20))

        tree = BinaryOperation(a, op, b)
        tree = folder.visit(tree)

        shouldBe = 0

        if op in ["&&", "||"]:
            shouldBe = \
                eval((str(a.value) + op + str(b.value)).
                     replace("&&", " and ").replace("||", " or "))
            shouldBe = int(bool(shouldBe))
        else:
            shouldBe = int(eval(str(a.value) + op + str(b.value)))

        assert tree == Number(shouldBe)


def testUnary():

    folder = ConstantFolder()

    operations = ["-", "!"]

    for op in operations:
        x = Number(random.randint(1, 20))

        tree = UnaryOperation(op, x)
        tree = folder.visit(tree)

        shouldBe = 0

        if op == "-":
            shouldBe = -x.value
        else:
            shouldBe = int(bool(not x.value))

        assert tree == Number(shouldBe)


def testZeroNumber():

    operations = \
        ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||"]

    folder = ConstantFolder()
    ref = Reference("ref")

    for op in operations:
        for it in range(2):
            for val in range(2):
                x = Number(val)

                if val != 0:
                    x = Number(random.randint(1, 20))

                tree = BinaryOperation(x, op, ref)

                if it:
                    tree = BinaryOperation(ref, op, x)
                    if op in ["/", "%"] and x.value == 0:
                        x = Number(random.randint(1, 20))

                tree = folder.visit(tree)

                if op == "*" and val == 0:
                    assert tree == Number(0)
                else:
                    assert not isinstance(tree, Number)


def testUniqueRef():

    operations = \
        ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||"]

    folder = ConstantFolder()
    ref = Reference("ref")

    for op in operations:

        tree = BinaryOperation(ref, op, ref)
        tree = folder.visit(tree)

        if op == "-":
            assert tree == Number(0)
        else:
            assert not isinstance(tree, Number)


def myTests():
    testTwoNumbers()
    testUnary()
    testZeroNumber()
    testUniqueRef()

if __name__ == "__main__":
    myTests()
