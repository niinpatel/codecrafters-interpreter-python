import sys
from typing import Any


def print_error(message):
    sys.stderr.write(f"{message}\n")


class Token:
    def __init__(self, type, lexeme, literal, line):
        self.type = type
        self.lexeme = lexeme
        self.literal = literal
        self.line = line

    def __str__(self) -> str:
        return f"{self.type} {self.lexeme} {self.literal if self.literal is not None else 'null'}"


class Scanner:
    def __init__(self, source):
        self.source = source
        self.tokens = []
        self.start = 0
        self.current = 0
        self.line = 1
        self.had_error = False

        self.keywords = {
            "and": "AND",
            "class": "CLASS",
            "else": "ELSE",
            "false": "FALSE",
            "for": "FOR",
            "fun": "FUN",
            "if": "IF",
            "nil": "NIL",
            "or": "OR",
            "print": "PRINT",
            "return": "RETURN",
            "super": "SUPER",
            "this": "THIS",
            "true": "TRUE",
            "var": "VAR",
            "while": "WHILE",
        }

    def advance(self):
        self.current += 1
        return self.source[self.current - 1]

    def scan_token(self):
        c = self.advance()
        if c == "(":
            self.add_token("LEFT_PAREN")
        elif c == ")":
            self.add_token("RIGHT_PAREN")
        elif c == "{":
            self.add_token("LEFT_BRACE")
        elif c == "}":
            self.add_token("RIGHT_BRACE")
        elif c == ",":
            self.add_token("COMMA")
        elif c == ".":
            self.add_token("DOT")
        elif c == "-":
            self.add_token("MINUS")
        elif c == "+":
            self.add_token("PLUS")
        elif c == ";":
            self.add_token("SEMICOLON")
        elif c == "*":
            self.add_token("STAR")
        elif c == "!":
            if self.match("="):
                self.add_token("BANG_EQUAL")
            else:
                self.add_token("BANG")
        elif c == "=":
            if self.match("="):
                self.add_token("EQUAL_EQUAL")
            else:
                self.add_token("EQUAL")
        elif c == "<":
            if self.match("="):
                self.add_token("LESS_EQUAL")
            else:
                self.add_token("LESS")
        elif c == ">":
            if self.match("="):
                self.add_token("GREATER_EQUAL")
            else:
                self.add_token("GREATER")
        elif c == "/":
            if self.match("/"):
                # a comment goes until the end of the line
                while self.peek() != "\n" and not self.is_at_end():
                    self.advance()
            else:
                self.add_token("SLASH")
        elif c == " ":
            pass
        elif c == "\r":
            pass
        elif c == "\t":
            pass
        elif c == "\n":
            self.line += 1
        elif c == '"':
            self.string()
        elif c.isdigit():
            self.number()
        elif c.isalpha() or c == "_":
            self.identifier()
        else:
            self.error(self.line, f"Unexpected character: {c}")

    def error(self, line, message, where=""):
        self.had_error = True
        print_error(f"[line {line}] Error{where}: {message}")

    def identifier(self):
        while self.peek().isalnum() or self.peek() == "_":
            self.advance()
        text = self.source[self.start : self.current]

        type = self.keywords.get(text)
        if type is None:
            self.add_token("IDENTIFIER")
        else:
            self.add_token(type)

    def number(self):
        while self.peek().isdigit():
            self.advance()

        if self.peek() == "." and self.peek_next().isdigit():
            self.advance()
            while self.peek().isdigit():
                self.advance()

        value = float(self.source[self.start : self.current])

        self.add_token("NUMBER", value)

    def peek_next(self):
        if self.current + 1 >= len(self.source):
            return "\0"
        return self.source[self.current + 1]

    def string(self):
        while self.peek() != '"' and not self.is_at_end():
            if self.peek() == "\n":
                self.line += 1
            self.advance()

        if self.is_at_end():
            self.error(self.line, "Unterminated string.")
            return

        # the closing "
        self.advance()

        # add the string token removing the surrounding "
        self.add_token("STRING", self.source[self.start + 1 : self.current - 1])

    def peek(self):
        if self.is_at_end():
            return "\0"
        return self.source[self.current]

    def match(self, expected):
        if self.is_at_end() or self.source[self.current] != expected:
            return False
        self.current += 1
        return True

    def is_at_end(self):
        return self.current >= len(self.source)

    def add_token(self, type, literal=None):
        text = self.source[self.start : self.current]
        self.tokens.append(Token(type, text, literal, self.line))

    def scan(self) -> list[Token]:
        while not self.is_at_end():
            self.start = self.current
            self.scan_token()

        self.tokens.append(Token("EOF", "", None, self.line))
        return self.tokens


class ExpressionVisitor:
    def visit_binary_expression(self, expression: "BinaryExpression"):
        raise NotImplementedError("Subclass must implement abstract method")

    def visit_literal_expression(self, expression: "LiteralExpression"):
        raise NotImplementedError("Subclass must implement abstract method")

    def visit_unary_expression(self, expression: "UnaryExpression"):
        raise NotImplementedError("Subclass must implement abstract method")

    def visit_grouping_expression(self, expression: "GroupingExpression"):
        raise NotImplementedError("Subclass must implement abstract method")


class Expression:
    def accept(self, visitor: "ExpressionVisitor"):
        raise NotImplementedError("Subclass must implement abstract method")


class BinaryExpression(Expression):
    def __init__(self, left: Expression, operator: Token, right: Expression):
        self.left = left
        self.operator = operator
        self.right = right

    def accept(self, visitor: "ExpressionVisitor"):
        return visitor.visit_binary_expression(self)


class LiteralExpression(Expression):
    def __init__(self, value: Any):
        self.value = value

    def accept(self, visitor: "ExpressionVisitor"):
        return visitor.visit_literal_expression(self)


class UnaryExpression(Expression):
    def __init__(self, operator: Token, right: Expression):
        self.operator = operator
        self.right = right

    def accept(self, visitor: "ExpressionVisitor"):
        return visitor.visit_unary_expression(self)


class GroupingExpression(Expression):
    def __init__(self, expression: Expression):
        self.expression = expression

    def accept(self, visitor: "ExpressionVisitor"):
        return visitor.visit_grouping_expression(self)


class ExpressionPrinter(ExpressionVisitor):
    def visit_binary_expression(self, expression: BinaryExpression):
        return f"({expression.operator.lexeme} {expression.left.accept(self)} {expression.right.accept(self)})"

    def visit_literal_expression(self, expression: LiteralExpression):
        return f"{expression.value}"

    def visit_unary_expression(self, expression: UnaryExpression):
        return f"({expression.operator.lexeme} {expression.right.accept(self)})"

    def visit_grouping_expression(self, expression: GroupingExpression):
        return f"(group {expression.expression.accept(self)})"

    def print(self, expression: Expression):
        return expression.accept(self)


class ExpressionEvaluator(ExpressionVisitor):
    def visit_binary_expression(self, expression: BinaryExpression):
        left = expression.left.accept(self)
        right = expression.right.accept(self)
        operator = expression.operator.lexeme
        if operator == "+":
            return left + right
        elif operator == "-":
            return left - right
        elif operator == "*":
            return left * right
        elif operator == "/":
            result = left / right
            return int(result) if result.is_integer() else result
        elif operator == "==":
            return "true" if left == right else "false"
        elif operator == "!=":
            return "true" if left != right else "false"
        elif operator == ">":
            return "true" if left > right else "false"
        elif operator == ">=":
            return "true" if left >= right else "false"
        elif operator == "<":
            return "true" if left < right else "false"
        elif operator == "<=":
            return "true" if left <= right else "false"

    def visit_literal_expression(self, expression: LiteralExpression):
        if isinstance(expression.value, str):
            return expression.value
        if expression.value.is_integer():
            return int(expression.value)
        return float(expression.value)

    def visit_unary_expression(self, expression: UnaryExpression):
        right = expression.right.accept(self)
        operator = expression.operator.lexeme
        if operator == "-":
            if isinstance(right, int):
                return -right
            elif isinstance(right, float):
                return -right
            else:
                print_error("Operand must be a number.")
                exit(1)
        elif operator == "!":
            if right == "true":
                return "false"
            elif right == "false":
                return "true"
            elif right == "nil":
                return "true"
            return "true" if not right else "false"

    def visit_grouping_expression(self, expression: GroupingExpression):
        return expression.expression.accept(self)

    def evaluate(self, expression: Expression):
        return expression.accept(self)


class Parser:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.current = 0

    def expression(self):
        return self.equality()

    def equality(self):
        expression = self.comparison()

        while self.match("EQUAL_EQUAL") or self.match("BANG_EQUAL"):
            operator = self.previous()
            right = self.comparison()
            expression = BinaryExpression(expression, operator, right)

        return expression

    def comparison(self):
        expression = self.term()

        while (
            self.match("GREATER")
            or self.match("GREATER_EQUAL")
            or self.match("LESS")
            or self.match("LESS_EQUAL")
        ):
            operator = self.previous()
            right = self.term()
            expression = BinaryExpression(expression, operator, right)

        return expression

    def term(self):
        expression = self.factor()

        while self.match("MINUS") or self.match("PLUS"):
            operator = self.previous()
            right = self.factor()
            expression = BinaryExpression(expression, operator, right)

        return expression

    def factor(self):
        expression = self.unary()

        while self.match("STAR") or self.match("SLASH"):
            operator = self.previous()
            right = self.unary()
            expression = BinaryExpression(expression, operator, right)

        return expression

    def unary(self):
        if self.match("BANG") or self.match("MINUS"):
            operator = self.previous()
            right = self.unary()
            return UnaryExpression(operator, right)
        return self.primary()

    def primary(self):
        if self.match("TRUE"):
            return LiteralExpression("true")

        if self.match("FALSE"):
            return LiteralExpression("false")

        if self.match("NIL"):
            return LiteralExpression("nil")

        if self.match("THIS"):
            return LiteralExpression("this")

        if self.match("SUPER"):
            return LiteralExpression("super")

        if self.match("LEFT_PAREN"):
            expression = self.expression()
            self.consume("RIGHT_PAREN", "Expect ')' after expression.")
            return GroupingExpression(expression)

        self.current += 1

        if self.previous().type == "NUMBER" or self.previous().type == "STRING":
            return LiteralExpression(self.previous().literal)

        self.error(self.previous(), "Expect expression.")

    def consume(self, type, message):
        if self.check(type):
            return self.advance()

        self.error(self.peek(), message)

    def check(self, type):
        if self.is_at_end():
            return False
        return self.peek().type == type

    def advance(self):
        if not self.is_at_end():
            self.current += 1
        return self.previous()

    def is_at_end(self):
        return self.peek().type == "EOF"

    def previous(self):
        return self.tokens[self.current - 1]

    def peek(self):
        return self.tokens[self.current]

    def match(self, type):
        if self.is_at_end() or self.peek().type != type:
            return False

        self.current += 1
        return True

    def error(self, token, message):
        print_error(f"[line {token.line}] Error at '{token.lexeme}': {message}")
        exit(65)


def main():
    if len(sys.argv) < 3:
        print("Usage: ./your_program.sh tokenize <filename>", file=sys.stderr)
        exit(1)

    command = sys.argv[1]
    filename = sys.argv[2]

    with open(filename) as file:
        file_contents = file.read()

        if command == "tokenize":
            scanner = Scanner(file_contents)
            scanner.scan()
            for token in scanner.tokens:
                print(token)
            if scanner.had_error:
                exit(65)
            return

        if command == "parse":
            scanner = Scanner(file_contents)
            scanner.scan()
            if scanner.had_error:
                exit(65)
            tokens = scanner.tokens

            parser = Parser(tokens)
            expression = parser.expression()
            print(ExpressionPrinter().print(expression))
            return

        if command == "evaluate":
            scanner = Scanner(file_contents)
            scanner.scan()
            if scanner.had_error:
                exit(65)
            tokens = scanner.tokens

            parser = Parser(tokens)
            expression = parser.expression()
            print(ExpressionEvaluator().evaluate(expression))
            return

    print(f"Unknown command: {command}", file=sys.stderr)
    exit(1)


if __name__ == "__main__":
    main()
