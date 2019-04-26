from collections import namedtuple
import itertools


def try_next(g):
    try:
        return next(g)
    except StopIteration:
        return None


def peek(g):
    head = try_next(g)
    return head, itertools.chain([head], g) if head is not None else g


def peek_all(g):
    l = list(g)
    # print(l)
    return (f for f in l)


# SEE(john): http://ns.inria.fr/ast/sql/index.html
class SqlSyntaxError(Exception):
    pass


def syntax_error_at_token(token):
    return SqlSyntaxError('Unexpected {} {} at line {}:{}'
                          .format(TOKEN_NAMES[str(token.type)],
                                  token.value,
                                  token.line,
                                  token.char))

def syntax_eof():
    return SqlSyntaxError('Unexpected EOF')


def try_next_sql_token(tokens):
    token = try_next(tokens)
    if not token:
        raise syntax_eof()
    return token


def constants(name, d):
    return namedtuple(name, d.keys())(*list(d.values()))

# SEE(john): https://www.postgresql.org/docs/9.4/sql-keywords-appendix.html
KEYWORDS = ['a', 'abort', 'abs', 'absent', 'absolute', 'access',
            'according', 'action', 'ada', 'add', 'admin', 'after',
            'aggregate', 'all', 'allocate', 'also', 'alter', 'always',
            'analyse', 'analyze', 'and', 'any', 'are', 'array',
            'array_agg', 'array_max_cardinality', 'as', 'asc',
            'asensitive', 'assertion', 'assignment', 'asymmetric',
            'at', 'atomic', 'attribute', 'attributes',
            'authorization', 'avg', 'backward', 'base64', 'before',
            'begin', 'begin_frame', 'begin_partition', 'bernoulli',
            'between', 'bigint', 'binary', 'bit', 'bit_length',
            'blob', 'blocked', 'bom', 'boolean', 'both', 'breadth',
            'by', 'c', 'cache', 'call', 'called', 'cardinality',
            'cascade', 'cascaded', 'case', 'cast', 'catalog',
            'catalog_name', 'ceil', 'ceiling', 'chain', 'char',
            'character', 'characteristics', 'characters',
            'character_length', 'character_set_catalog',
            'character_set_name', 'character_set_schema',
            'char_length', 'check', 'checkpoint', 'class',
            'class_origin', 'clob', 'close', 'cluster', 'coalesce',
            'cobol', 'collate', 'collation', 'collation_catalog',
            'collation_name', 'collation_schema', 'collect', 'column',
            'columns', 'column_name', 'command_function',
            'command_function_code', 'comment', 'comments', 'commit',
            'committed', 'concurrently', 'condition',
            'condition_number', 'configuration', 'connect',
            'connection', 'connection_name', 'constraint',
            'constraints', 'constraint_catalog', 'constraint_name',
            'constraint_schema', 'constructor', 'contains', 'content',
            'continue', 'control', 'conversion', 'convert', 'copy',
            'corr', 'corresponding', 'cost', 'count', 'covar_pop',
            'covar_samp', 'create', 'cross', 'csv', 'cube',
            'cume_dist', 'current', 'current_catalog', 'current_date',
            'current_default_transform_group', 'current_path',
            'current_role', 'current_row', 'current_schema',
            'current_time', 'current_timestamp',
            'current_transform_group_for_type', 'current_user',
            'cursor', 'cursor_name', 'cycle', 'data', 'database',
            'datalink', 'date', 'datetime_interval_code',
            'datetime_interval_precision', 'day', 'db', 'deallocate',
            'dec', 'decimal', 'declare', 'default', 'defaults',
            'deferrable', 'deferred', 'defined', 'definer', 'degree',
            'delete', 'delimiter', 'delimiters', 'dense_rank',
            'depth', 'deref', 'derived', 'desc', 'describe',
            'descriptor', 'deterministic', 'diagnostics',
            'dictionary', 'disable', 'discard', 'disconnect',
            'dispatch', 'distinct', 'dlnewcopy', 'dlpreviouscopy',
            'dlurlcomplete', 'dlurlcompleteonly',
            'dlurlcompletewrite', 'dlurlpath', 'dlurlpathonly',
            'dlurlpathwrite', 'dlurlscheme', 'dlurlserver', 'dlvalue',
            'do', 'document', 'domain', 'double', 'drop', 'dynamic',
            'dynamic_function', 'dynamic_function_code', 'each',
            'element', 'else', 'empty', 'enable', 'encoding',
            'encrypted', 'end', 'end-exec', 'end_frame',
            'end_partition', 'enforced', 'enum', 'equals', 'escape',
            'event', 'every', 'except', 'exception', 'exclude',
            'excluding', 'exclusive', 'exec', 'execute', 'exists',
            'exp', 'explain', 'expression', 'extension', 'external',
            'extract', 'false', 'family', 'fetch', 'file', 'filter',
            'final', 'first', 'first_value', 'flag', 'float', 'floor',
            'following', 'for', 'force', 'foreign', 'fortran',
            'forward', 'found', 'frame_row', 'free', 'freeze', 'from',
            'fs', 'full', 'function', 'functions', 'fusion', 'g',
            'general', 'generated', 'get', 'global', 'go', 'goto',
            'grant', 'granted', 'greatest', 'group', 'grouping',
            'groups', 'handler', 'having', 'header', 'hex',
            'hierarchy', 'hold', 'hour', 'id', 'identity', 'if',
            'ignore', 'ilike', 'immediate', 'immediately',
            'immutable', 'implementation', 'implicit', 'import', 'in',
            'including', 'increment', 'indent', 'index', 'indexes',
            'indicator', 'inherit', 'inherits', 'initially', 'inline',
            'inner', 'inout', 'input', 'insensitive', 'insert',
            'instance', 'instantiable', 'instead', 'int', 'integer',
            'integrity', 'intersect', 'intersection', 'interval',
            'into', 'invoker', 'is', 'isnull', 'isolation', 'join',
            'k', 'key', 'key_member', 'key_type', 'label', 'lag',
            'language', 'large', 'last', 'last_value', 'lateral',
            'lc_collate', 'lc_ctype', 'lead', 'leading', 'leakproof',
            'least', 'left', 'length', 'level', 'library', 'like',
            'like_regex', 'limit', 'link', 'listen', 'ln', 'load',
            'local', 'localtime', 'localtimestamp', 'location',
            'locator', 'lock', 'lower', 'm', 'map', 'mapping',
            'match', 'matched', 'materialized', 'max', 'maxvalue',
            'max_cardinality', 'member', 'merge', 'message_length',
            'message_octet_length', 'message_text', 'method', 'min',
            'minute', 'minvalue', 'mod', 'mode', 'modifies', 'module',
            'month', 'more', 'move', 'multiset', 'mumps', 'name',
            'names', 'namespace', 'national', 'natural', 'nchar',
            'nclob', 'nesting', 'new', 'next', 'nfc', 'nfd', 'nfkc',
            'nfkd', 'nil', 'no', 'none', 'normalize', 'normalized',
            'not', 'nothing', 'notify', 'notnull', 'nowait',
            'nth_value', 'ntile', 'null', 'nullable', 'nullif',
            'nulls', 'number', 'numeric', 'object',
            'occurrences_regex', 'octets', 'octet_length', 'of',
            'off', 'offset', 'oids', 'old', 'on', 'only', 'open',
            'operator', 'option', 'options', 'or', 'order',
            'ordering', 'ordinality', 'others', 'out', 'outer',
            'output', 'over', 'overlaps', 'overlay', 'overriding',
            'owned', 'owner', 'p', 'pad', 'parameter',
            'parameter_mode', 'parameter_name',
            'parameter_ordinal_position',
            'parameter_specific_catalog', 'parameter_specific_name',
            'parameter_specific_schema', 'parser', 'partial',
            'partition', 'pascal', 'passing', 'passthrough',
            'password', 'path', 'percent', 'percentile_cont',
            'percentile_disc', 'percent_rank', 'period', 'permission',
            'placing', 'plans', 'pli', 'portion', 'position',
            'position_regex', 'power', 'precedes', 'preceding',
            'precision', 'prepare', 'prepared', 'preserve', 'primary',
            'prior', 'privileges', 'procedural', 'procedure',
            'program', 'public', 'quote', 'range', 'rank', 'read',
            'reads', 'real', 'reassign', 'recheck', 'recovery',
            'recursive', 'ref', 'references', 'referencing',
            'refresh', 'regr_avgx', 'regr_avgy', 'regr_count',
            'regr_intercept', 'regr_r2', 'regr_slope', 'regr_sxx',
            'regr_sxy', 'regr_syy', 'reindex', 'relative', 'release',
            'rename', 'repeatable', 'replace', 'replica', 'requiring',
            'reset', 'respect', 'restart', 'restore', 'restrict',
            'result', 'return', 'returned_cardinality',
            'returned_length', 'returned_octet_length',
            'returned_sqlstate', 'returning', 'returns', 'revoke',
            'right', 'role', 'rollback', 'rollup', 'routine',
            'routine_catalog', 'routine_name', 'routine_schema',
            'row', 'rows', 'row_count', 'row_number', 'rule',
            'savepoint', 'scale', 'schema', 'schema_name', 'scope',
            'scope_catalog', 'scope_name', 'scope_schema', 'scroll',
            'search', 'second', 'section', 'security', 'select',
            'selective', 'self', 'sensitive', 'sequence', 'sequences',
            'serializable', 'server', 'server_name', 'session',
            'session_user', 'set', 'setof', 'sets', 'share', 'show',
            'similar', 'simple', 'size', 'smallint', 'snapshot',
            'some', 'source', 'space', 'specific', 'specifictype',
            'specific_name', 'sql', 'sqlcode', 'sqlerror',
            'sqlexception', 'sqlstate', 'sqlwarning', 'sqrt',
            'stable', 'standalone', 'start', 'state', 'statement',
            'static', 'statistics', 'stddev_pop', 'stddev_samp',
            'stdin', 'stdout', 'storage', 'strict', 'strip',
            'structure', 'style', 'subclass_origin', 'submultiset',
            'substring', 'substring_regex', 'succeeds', 'sum',
            'symmetric', 'sysid', 'system', 'system_time',
            'system_user', 't', 'table', 'tables', 'tablesample',
            'tablespace', 'table_name', 'temp', 'template',
            'temporary', 'text', 'then', 'ties', 'time', 'timestamp',
            'timezone_hour', 'timezone_minute', 'to', 'token',
            'top_level_count', 'trailing', 'transaction',
            'transactions_committed', 'transactions_rolled_back',
            'transaction_active', 'transform', 'transforms',
            'translate', 'translate_regex', 'translation', 'treat',
            'trigger', 'trigger_catalog', 'trigger_name',
            'trigger_schema', 'trim', 'trim_array', 'true',
            'truncate', 'trusted', 'type', 'types', 'uescape',
            'unbounded', 'uncommitted', 'under', 'unencrypted',
            'union', 'unique', 'unknown', 'unlink', 'unlisten',
            'unlogged', 'unnamed', 'unnest', 'until', 'untyped',
            'update', 'upper', 'uri', 'usage', 'user',
            'user_defined_type_catalog', 'user_defined_type_code',
            'user_defined_type_name', 'user_defined_type_schema',
            'using', 'vacuum', 'valid', 'validate', 'validator',
            'value', 'values', 'value_of', 'varbinary', 'varchar',
            'variadic', 'varying', 'var_pop', 'var_samp', 'verbose',
            'version', 'versioning', 'view', 'views', 'volatile',
            'when', 'whenever', 'where', 'whitespace', 'width_bucket',
            'window', 'with', 'within', 'without', 'work', 'wrapper',
            'write', 'xml', 'xmlagg', 'xmlattributes', 'xmlbinary',
            'xmlcast', 'xmlcomment', 'xmlconcat', 'xmldeclaration',
            'xmldocument', 'xmlelement', 'xmlexists', 'xmlforest',
            'xmliterate', 'xmlnamespaces', 'xmlparse', 'xmlpi',
            'xmlquery', 'xmlroot', 'xmlschema', 'xmlserialize',
            'xmltable', 'xmltext', 'xmlvalidate', 'year', 'yes',
            'zone']

OPERATORS = ['::', '<@']
TOKEN_TYPES = {
    'UNKNOWN': -1,
    'PAREN_OPEN': 0,  # (
    'PAREN_CLOSE': 1,  # )
    'COLON': 2,  # :
    'SEMICOLON': 3,  # ;
    'ASTERISK': 4,  # *
    'BRACKET_OPEN': 5,  # [
    'BRACKET_CLOSE': 6,  # ]
    'BRACE_OPEN': 7,  # {
    'BRACE_CLOSE': 8,  # }
    'STRING': 9,  # 'something'
    'IDENTIFIER': 10,  # created_at, and other column names
    'OPERATOR': 11,  # ->, -->, <@}, <>, etc.
    'NUMBER': 12,  # 1234
    'PERIOD': 13,  # .
    'COMMA': 14,  # ,
    'KEYWORD': 15,  # SELECT, UNION, UPDATE, etc.
    'PARAMETER': 16,  # NOTE(john): begins with a :, as in :foo
}
TOKEN_NAMES = {}

for k in TOKEN_TYPES:
    TOKEN_NAMES[str(TOKEN_TYPES[k])] = k

# LEXING
Token = namedtuple('TOKEN', 'type value position line char')
TOKENS = constants('TOKENS', TOKEN_TYPES)
# PARSING
SQLScript = namedtuple('SQLScript', 'parameters statements')
SQLSelect = namedtuple('SQLSelect', 'token columns clauses')
SQLUpdate = namedtuple('SQLUpdate', 'token columns clauses')
SQLInsert = namedtuple('SQLInsert', 'token table columns values')
SQLColumn = namedtuple('SQLColumn', 'tokens schema table column identifier expression')
SQLExpression = namedtuple('SQLExpression', 'token children')


def char_to_token(c):
    if c.isspace():
        return None
    elif c.isalpha():
        return TOKENS.IDENTIFIER
    elif c == '"':
        return TOKENS.IDENTIFIER
    elif c == ':':
        return TOKENS.IDENTIFIER
    elif c == '\'':
        return TOKENS.STRING
    elif c in '+-*/<>=~!@#%^&|`?':
        return TOKENS.OPERATOR
    elif c.isnumeric():
        return TOKENS.NUMBER
    elif c == '(':
        return TOKENS.PAREN_OPEN
    elif c == ')':
        return TOKENS.PAREN_CLOSE
    elif c == ';':
        return TOKENS.SEMICOLON
    elif c == '.':
        return TOKENS.PERIOD
    elif c == ',':
        return TOKENS.COMMA


def lex_sql(script):
    tokens = []
    state = TOKENS.UNKNOWN
    buffered_token_value = ''
    token_start_position = -1
    line = 1
    line_offset = 0
    position = -1
    for c in script:
        position += 1
        line_offset += 1
        if state == TOKENS.UNKNOWN:
            if c.isspace():
                pass
            else:
                state = char_to_token(c)

            if state in [ TOKENS.IDENTIFIER, TOKENS.STRING,
                          TOKENS.OPERATOR, TOKENS.NUMBER ]:
                buffered_token_value += c
                token_start_position = position
            elif state in [ TOKENS.SEMICOLON, TOKENS.PERIOD, TOKENS.COMMA,
                            TOKENS.PAREN_OPEN, TOKENS.PAREN_CLOSE ]:
                tokens.append(Token(
                    type=state,
                    value=c,
                    position=position,
                    line=line,
                    char=line_offset
                ))
                buffered_token_value = ''
                state = TOKENS.UNKNOWN
        elif (state in
              [ TOKENS.STRING,
                TOKENS.IDENTIFIER,
                TOKENS.OPERATOR,
                TOKENS.NUMBER ]):
            token_complete = False
            current_char_token = TOKENS.UNKNOWN

            if c.isspace():
                token_complete = True
            elif c in '=<>-+*/;().,':
                token_complete = True
                current_char_token = char_to_token(c)

            if token_complete:
                if buffered_token_value.lower() in KEYWORDS:
                    state = TOKENS.KEYWORD
                elif buffered_token_value.startswith(':'):
                    state = TOKENS.PARAMETER
                tokens.append(Token(
                    type=state,
                    value=buffered_token_value,
                    position=token_start_position,
                    line=line,
                    char=line_offset
                ))
                buffered_token_value = ''

                if current_char_token in [
                        TOKENS.SEMICOLON, TOKENS.PERIOD, TOKENS.COMMA,
                        TOKENS.PAREN_OPEN, TOKENS.PAREN_CLOSE ]:
                    tokens.append(Token(
                        type=char_to_token(c),
                        value=c,
                        position=position,
                        line=line,
                        char=line_offset
                    ))

                state = TOKENS.UNKNOWN
            else:
                buffered_token_value += c
        if c == '\n':
            line += 1
            line_offset = 0

    return tokens


def parse_column(tokens):
    """Example:
    1,
    created_at,
    count(*),
    *,
    total / 100.0 AS percentage
    """
    column_tokens = []
    expression_tokens = []
    current_token = try_next_sql_token(tokens)
    in_column = True
    identifier = ''
    buffered_tokens = []

    if current_token.type == TOKENS.COMMA:
        current_token = try_next_sql_token(tokens)

    while in_column:
        if not current_token:
            raise syntax_eof()

        if current_token.type == TOKENS.SEMICOLON:
            in_column = False
            buffered_tokens.append(current_token)
            continue

        if len(column_tokens) == 1:
            if current_token.type in [ TOKENS.COMMA, TOKENS.SEMICOLON ]:
                in_column = False
                buffered_tokens.append(current_token)
                tokens = itertools.chain(buffered_tokens, tokens)
                identifier = column_tokens[0].value
                continue

        if current_token.type in [ TOKENS.COMMA, TOKENS.SEMICOLON ]:
            in_column = False
            buffered_tokens.append(current_token)
            tokens = itertools.chain(buffered_tokens, tokens)
            identifier = column_tokens[0].value
            continue

        if current_token.type == TOKENS.PAREN_OPEN:
            in_expression = True
            while in_expression:
                column_tokens.append(current_token)
                current_token = try_next_sql_token(tokens)
                if current_token.type == TOKENS.PAREN_CLOSE:
                    in_expression = False
            continue

        if current_token.type == TOKENS.KEYWORD and current_token.value.lower() == 'as':
            column_tokens.append(current_token)
            current_token = try_next_sql_token(tokens)
            if current_token.type == TOKENS.IDENTIFIER:
                column_tokens.append(current_token)
                identifier = current_token.value
                current_token = try_next_sql_token(tokens)
                if current_token.type in [ TOKENS.COMMA, TOKENS.SEMICOLON ]:
                    in_column = False
                    buffered_tokens.append(current_token)
                    continue
                else:
                    raise syntax_error_at_token(current_token)

        if current_token.type == TOKENS.KEYWORD and current_token.value.lower() in [
                'select', 'where', 'update', 'from', 'group', 'having', 'into', 'order' ]:
            in_column = False
            buffered_tokens.append(current_token)
            continue

        column_tokens.append(current_token)
        current_token = try_next_sql_token(tokens)

    column = SQLColumn(tokens=column_tokens, identifier=identifier, expression=expression_tokens,
                       column='', schema='', table='')
    return column, itertools.chain(buffered_tokens, tokens)


def parse_from(tokens):
    """Example:
    FROM <expression>

    FROM foo inner join bar on foo.a = bar.a
    """
    pass


def parse_where(tokens):
    """Example:
    WHERE <expressions> [AND|OR|EXISTS|IN|BETWEEN]
    """
    pass


def parse_order_by(tokens):
    """Example:
    ORDER BY <expression>, <expression>
    """
    pass


def parse_limit(tokens):
    """Example:
    LIMIT 25;
    """
    pass


def parse_group_by(tokens):
    """Example:
    GROUP BY <expression> <direction>, <expression> <direction>
    """
    pass


def parse_having(tokens):
    """Example:
    HAVING <expression>, <expression>
    """
    pass


def parse_select(tokens):
    """Example:
    SELECT
       /column_1/, /column_n/
    /clauses/
    """
    current_token = next(tokens)
    select_token = current_token

    if current_token.type == TOKENS.KEYWORD and current_token.value == 'SELECT':
        columns = []
        in_columns = True
        while in_columns:
            column, tokens = parse_column(tokens)
            tokens = peek_all(tokens)
            columns.append(column)
            head, tokens = peek(tokens)
            if head is None:
                raise syntax_eof()
            if head.type in [ TOKENS.SEMICOLON ]:
                in_columns = False
                continue
            if head.type == TOKENS.KEYWORD and head.value.lower() in [
                    'from', 'where', 'group', 'having', 'order', 'into' ]:
                in_columns = False
                continue
            current_token = next(tokens)
        
        return [SQLSelect(token=select_token, columns=columns, clauses=[])]
    else:
        raise syntax_error_at_token(head)


def find_tokens_by_type(tokens, token_type):
    results = []
    buffered_tokens = []
    current_token = try_next(tokens)
    while current_token:
        buffered_tokens.append(current_token)
        if current_token.type == token_type:
            results.append(current_token)
        current_token = try_next(tokens)
    return results, itertools.chain(buffered_tokens, tokens)


def parse_ast(tokens):
    parameters, tokens = find_tokens_by_type(tokens, TOKENS.PARAMETER)
    
    head, tokens = peek(tokens)
    statements = []

    if head.type == TOKENS.KEYWORD:
        if head.value.lower() == 'select':
            statements.append(parse_select(tokens))
        else:
            raise Exception('Stay tuned')

    return SQLScript(parameters=parameters,
                     statements=statements)


def get_column_from_parameter(param):
    return param.value[1:]


def generate_python_code(script, name):
    token_stream = (t for t in lex_sql(script))
    ast = parse_ast(token_stream)
    params = ast.parameters.copy()
    params.reverse()

    for parameter in params:
        script = '{}%s{}'.format(script[:parameter.position],
                                 script[parameter.position + len(parameter.value):])

    parameter_list = ', '.join(map(get_column_from_parameter, ast.parameters))
    cursor_parameter_list = parameter_list

    if len(ast.parameters) == 1:
        cursor_parameter_list = '{},'.format(get_column_from_parameter(ast.parameters[0]))

    whole_script = """
def {}(conn, {}):
  with conn.cursor() as cursor:
    cursor.execute({}
{}
    {}, ({}))
    return cursor.fetchmany()
    """
    result = whole_script.format(
        name,
        parameter_list,
        '"""',
        script,
        '"""',
        cursor_parameter_list)

    return result


testing = 1
if testing:
    sample_script = """
    SELECT
      widget_id,
      widget_type,
      (SELECT count(*)
       FROM widget w
       WHERE w.parent_id = widget.widget_id) as 'children_count',
      created_at
    FROM
      widget
    WHERE
      active = :active or
      prev_active is not null or
      owner_id = :owner_id
    LIMIT 25;
    """
    def run():
        # tokens = lex_sql(sample_script)
        # for token in tokens:
        #     print('{:10} {:10}\t{}'
        #           .format('({},{})'.format(token.line, token.position),
        #                   TOKEN_NAMES[str(token.type)],
        #                   token.value))
        tokens = (t for t in lex_sql(sample_script))
        result = parse_select(tokens)
        print(result)
