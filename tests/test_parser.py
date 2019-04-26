#!/usr/bin python3
import unittest
from src.norman import (
    lex_sql,
    parse_ast,
    parse_select,
    TOKENS,
    generate_python_code
)


class TestParser(unittest.TestCase):
    def test_lexer(self):
        result = lex_sql("""SELECT * FROM foo;""")
        t_select, t_asterisk, t_from, t_foo, t_semi = result
        assert t_select.value == 'SELECT'
        assert t_select.type == TOKENS.KEYWORD
        assert t_asterisk.value == '*'
        assert t_asterisk.type == TOKENS.OPERATOR
        assert t_from.value == 'FROM'
        assert t_from.type == TOKENS.KEYWORD
        assert t_foo.value == 'foo'
        assert t_foo.type == TOKENS.IDENTIFIER
        assert t_semi.value == ';'
        assert t_semi.type == TOKENS.SEMICOLON

    def test_parser_select_one(self):
        tokens = lex_sql("""SELECT 1;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]
        assert select_statement
        assert select_statement.token.value == 'SELECT'
        assert select_statement.token.type == TOKENS.KEYWORD

    def test_parser_select_one(self):
        tokens = lex_sql("""SELECT 1;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert column_1.tokens[0].value == '1', column_1
        assert column_1.identifier == '', column_1

    def test_parser_select_one_alias(self):
        tokens = lex_sql("""SELECT 1 as one;""")
        # print(tokens)
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert column_1.tokens[0].value == '1', column_1
        assert column_1.identifier == 'one', column_1

    def test_parser_select_count(self):
        tokens = lex_sql("""SELECT COUNT(*);""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert column_1.tokens[0].value == 'COUNT', column_1
        assert column_1.identifier == '', column_1

    def test_parser_select_count_alias(self):
        tokens = lex_sql("""SELECT COUNT(*) as ONE;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert column_1.tokens[0].value == 'COUNT', column_1
        assert column_1.identifier == 'ONE', column_1

    def test_parser_select_subselect(self):
        tokens = lex_sql("""SELECT (SELECT 1);""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert column_1.tokens[0].value == '(', column_1
        assert column_1.identifier == '', column_1

    def test_parser_select_subselect_alias(self):
        tokens = lex_sql("""SELECT (SELECT 1) as ONE;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert column_1.tokens[0].value == '(', column_1
        assert column_1.identifier == 'ONE', column_1

    def test_parser_select_expression(self):
        tokens = lex_sql("""SELECT 1 + 1;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert len(column_1.tokens) == 3, column_1.tokens
        assert column_1.tokens[0].value == '1', column_1
        assert column_1.identifier == '', column_1

    def test_parser_select_expression_alias(self):
        tokens = lex_sql("""SELECT 1 + 1 as ONE;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]

        # TODO(john): This includes 'as ONE', is that right? I'm
        # thinking it is, but the expression field should have a
        # parsed representation (which will naturally omit it,
        # probably?)
        assert len(column_1.tokens) == 5, column_1.tokens
        assert column_1.tokens[0].value == '1', column_1
        assert column_1.identifier == 'ONE', column_1

    def test_parser_select_expression_with_subquery(self):
        tokens = lex_sql("""SELECT 1 + (SELECT 1);""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert len(column_1.tokens) == 6, column_1.tokens
        assert column_1.tokens[0].value == '1', column_1
        assert column_1.identifier == '', column_1

    def test_parser_select_expression_with_subquery_alias(self):
        tokens = lex_sql("""SELECT 1 + (SELECT 1) as ONE;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert len(column_1.tokens) == 8, column_1.tokens
        assert column_1.tokens[0].value == '1', column_1
        assert column_1.identifier == 'ONE', column_1

    def test_parser_select_expression_with_count(self):
        tokens = lex_sql("""SELECT 1 + count(*);""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert len(column_1.tokens) == 6, column_1.tokens
        assert column_1.tokens[0].value == '1', column_1
        assert column_1.identifier == '', column_1

    def test_parser_select_expression_with_count_alias(self):
        tokens = lex_sql("""SELECT 1 + COUNT(*) as ONE;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]

        select_columns = select_statement.columns
        assert len(select_columns) == 1, \
            'Expected {} select_columns but got {}'\
            .format(1, len(select_columns))
        column_1 = select_columns[0]
        assert len(column_1.tokens) == 8, column_1.tokens
        assert column_1.tokens[0].value == '1', column_1
        assert column_1.identifier == 'ONE', column_1

    # 1,                   NUMBER COMMA
    # 1;                   NUMBER SEMICOLON
    # '1',                 STRING COMMA
    # 1 AS ONE,            IDENTIFIER KEYWORD IDENTIFIER COMMA
    # one,                 IDENTIFIER COMMA
    # COUNT(*) AS total,   KEYWORD PAREN_OPEN OPERATOR PAREN_CLOSE KEYWORD IDENTIFIER COMMA
    # 1 + 1 AS total,      NUMBER OPERATOR NUMBER KEYWORD IDENTIFIER COMMA
    def test_parser_select_two(self):
        tokens = lex_sql("""SELECT 1, 2;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]
        assert select_statement
        select_columns = select_statement.columns
        # print(select_columns)
        assert len(select_columns) == 2, \
            'Expected {} select_columns but got {}'\
            .format(2, len(select_columns))
        column_1 = select_columns[0]
        assert column_1.tokens[0].value == '1', column_1
        column_2 = select_columns[1]
        assert column_2.tokens[0].value == '2', column_2

    def test_parser_select_two_alias(self):
        tokens = lex_sql("""SELECT 1 as one, 2 as two;""")
        token_stream = (t for t in tokens)
        result = parse_select(token_stream)
        select_statement = result[0]
        assert select_statement
        select_columns = select_statement.columns
        # print(select_columns)
        assert len(select_columns) == 2, \
            'Expected {} select_columns but got {}'\
            .format(2, len(select_columns))
        column_1 = select_columns[0]
        assert column_1.tokens[0].value == '1', column_1
        assert column_1.identifier == 'one', column_1
        column_2 = select_columns[1]
        assert column_2.tokens[0].value == '2', column_2
        assert column_2.identifier == 'two', column_1

    def test_parser_ast(self):
        tokens = lex_sql("""SELECT * FROM FOO WHERE foo_id = :foo_id;""")
        token_stream = (t for t in tokens)
        result = parse_ast(token_stream)
        assert len(result.statements) == 1
        assert len(result.parameters) == 1

    def test_generate_python_code(self):
        generate_python_code("""SELECT * FROM FOO WHERE foo_id = :foo_id;""", 'get_foo')

if __name__ == 'main':
    unittest.main(exit=True, buffer=True)
