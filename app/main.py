import sys


def print_error(message):
    sys.stderr.write(f'"{message}"\n')


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
            self.add_token("BANG")
        elif c == "=":
            self.add_token("EQUAL")
        elif c == "<":
            self.add_token("LESS")
        elif c == ">":
            self.add_token("GREATER")
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
        elif c.isalpha():
            self.identifier()
        else:
            self.error(self.line, f"Unexpected character: {c}")

    def error(self, line, message, where=""):
        self.had_error = True
        print_error(f"[line {line}] Error{where}: {message}")

    def identifier(self):
        while self.peek().isalnum():
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

        self.add_token("NUMBER", float(self.source[self.start : self.current]))

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
            self.error(self.line, "Unterminated string")

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


def main():
    if len(sys.argv) < 3:
        print("Usage: ./your_program.sh tokenize <filename>", file=sys.stderr)
        exit(1)

    command = sys.argv[1]
    filename = sys.argv[2]

    if command != "tokenize":
        print(f"Unknown command: {command}", file=sys.stderr)
        exit(1)

    with open(filename) as file:
        file_contents = file.read()

    # Uncomment this block to pass the first stage
    if file_contents:
        scanner = Scanner(file_contents)
        scanner.scan()
        for token in scanner.tokens:
            print(token)
        if scanner.had_error:
            exit(65)
    else:
        print(
            "EOF  null"
        )  # Placeholder, remove this line when implementing the scanner


if __name__ == "__main__":
    main()
