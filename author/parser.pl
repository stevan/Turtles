

# Operator precedence table (higher number = tighter binding)
our $PRECEDENCE = {
    # Lambda - absolutely lowest precedence
    '->'  => 0.1,    # lambda: x -> body (right-associative)

    # Sequencing
    ';'   => 0.5,    # sequence: evaluate left, then right

    # List construction
    ','   => 1,      # CONS for lists

    # Pair construction
    ':'   => 5,      # pair/tuple: key:value

    # Logical OR
    '||'  => 10,     # logical or

    # Logical AND
    '&&'  => 20,     # logical and

    # Equality comparison
    '=='  => 30,     # equal
    '!='  => 30,     # not equal

    # Relational comparison
    '<'   => 40,     # less than
    '>'   => 40,     # greater than
    '<='  => 40,     # less than or equal
    '>='  => 40,     # greater than or equal

    # Addition/Subtraction
    '+'   => 50,     # addition
    '-'   => 50,     # subtraction

    # Multiplication/Division/Modulo
    '*'   => 60,     # multiplication
    '/'   => 60,     # division
    '%'   => 60,     # modulo

    # Exponentiation (right-associative)
    '**'  => 70,     # power

    # Member access - highest precedence
    '.'   => 80,     # property/method access
};



class Parser {
    field $tokens     :param :reader;
    field $precedence :param :reader;
    field $pos = 0;

    method parse {
        my $result = $self->parse_expr(0);
        die "Unexpected tokens remaining" unless $self->at_end();
        return $result;
    }

    # Core Pratt parsing: parse expression with minimum binding power
    method parse_expr ($min_bp) {
        # 1. Parse the left side (nud - null denotation)
        my $left = $self->nud();

        # 2. Keep consuming operators while they bind tighter than min_bp
        while (!$self->at_end() && $self->get_bp() > $min_bp) {
            my $op = $self->current_token();
            $left = $self->led($left, $op);
        }

        return $left;
    }

    # NUD: Null denotation - handle token at start of expression
    method nud {
        my $token = $self->consume();

        # Handle numbers
        if ($self->is_number($token)) {
            return Corgi::AST::Term->new(name => 'const', value => $token);
        }

        # Handle VOID/HALT token
        if ($token eq 'VOID' || $token eq 'HALT') {
            return Corgi::AST::Void->new(name => 'void');
        }

        # Handle identifiers (for property access, variables, etc)
        if ($self->is_identifier($token)) {
            return Corgi::AST::Term->new(name => 'ident', value => $token);
        }

        # Could add prefix operators here (e.g., unary -)
        die "Unexpected token at position " . ($pos - 1) . ": $token";
    }

    # LED: Left denotation - handle infix operator with left side already parsed
    method led ($left, $op) {
        $self->consume();  # eat the operator

        my $bp = $precedence->{$op};
        die "Unknown operator: $op" unless defined $bp;

        # Right-associative operators: **, ->
        # For right-associative: pass $bp - 1 (same precedence continues)
        # For left-associative: pass $bp (same precedence stops)
        my $is_right_assoc = ($op eq '**' || $op eq '->');
        my $right = $self->parse_expr($is_right_assoc ? $bp - 1 : $bp);

        return Corgi::AST::BinOp->new(
            name  => $op,
            first => $left,
            other => $right
        );
    }

    method current_token { $tokens->[$pos] }
    method consume { $tokens->[$pos++] }
    method at_end { $pos >= scalar @$tokens }
    method get_bp {
        return 0 if $self->at_end();
        my $token = $self->current_token();
        return $precedence->{$token} // 0;
    }
    method is_number ($token) { $token =~ /^-?\d+$/ }
    method is_identifier ($token) { $token =~ /^[a-zA-Z_][a-zA-Z0-9_]*$/ }
}
