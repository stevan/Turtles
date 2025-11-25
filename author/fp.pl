#!perl

use v5.40;
use experimental qw[ class switch ];

use List::Util ();

class Term {
    use overload '""' => 'to_string';
    method to_string;
}

class Exception :isa(Term) {
    field $msg  :param :reader;
    field $expr :param :reader;

    sub throw ($, $m, $e, @rest) {
        $m = sprintf( $m => @rest ) if @rest;
        return Exception->new( msg => $m, expr => $e )
    }

    method to_string (@) {
        sprintf 'Exception(%s) at %s' => $msg, $expr
    }
}

class Bool :isa(Term) {
    field $value :param :reader;
    method to_string (@) { $value ? '#t' : '#f' }
}

class Num :isa(Term) {
    field $value :param :reader;
    method to_string (@) { $value."" }
}

class Var :isa(Term) {
    field $ident :param :reader;
    method to_string (@) { $ident }
}

class List :isa(Term) {
    method head;
    method tail;
}

class List::Nil :isa(List) {
    method head { Exception->throw('cannot call head on nil', $self) }
    method tail { Exception->throw('cannot call tail on nil', $self) }

    method to_string (@) { '()' }
}

class List::Cons :isa(List) {
    field $head :param :reader;
    field $tail :param :reader;

    method to_string (@) {
        sprintf '(%s %s)' => $head, $tail;
    }
}

class App :isa(List::Cons) {}

class Abs :isa(List::Cons) {}

class Abs::Closure :isa(Abs) {
    method to_string (@) {
        sprintf '/(%s -> %s)' => $self->head, $self->tail;
    }
}

class Abs::Lambda :isa(Abs) {
    method to_string (@) {
        sprintf '(\%s. %s)' => $self->head, $self->tail;
    }
}

class Abs::Native :isa(Abs) {
    method to_string (@) {
        sprintf '(\%s. &%s)' => => $self->head, (refaddr $self->tail) =~ /(....)/;
    }
}

class Abs::FExpr :isa(Abs) {
    method to_string (@) {
        sprintf '(^%s. &%s)' => => $self->head, (refaddr $self->tail) =~ /(....)/;
    }
}

class Env {
    use overload '""' => 'to_string';

    our $ID_SEQ = 0;
    field $id :reader = ++$ID_SEQ;

    field $parent :param :reader = undef;
    field $locals :param :reader;

    method get    ($id)       { $locals->{ $id->ident } }
    method set    ($id, $val) { $locals->{ $id->ident } = $val }
    method has    ($id)       { exists $locals->{ $id->ident } }

    method exists ($id) { $self->has($id) // ($parent ? $parent->has($id) : false) }
    method lookup ($id) { $self->get($id) // ($parent ? $parent->get($id) : undef) }

    method derive (%with) { Env->new( parent => $self, locals => \%with) }

    method parent_id { $parent ? $parent->id : 0 }

    method to_string (@) {
        sprintf '%%E(%02d:%02d)<%s>' => $id, $self->parent_id, join ', ' =>
            map { sprintf '%s : %s' => $_, $locals->{$_} }
                keys %$locals;
    }
}


sub env (%e) { Env->new( locals => \%e ) }

sub id ($x) { $x }

sub bool ($n) { Bool->new( value => $n ) }
sub num  ($n) {  Num->new( value => $n ) }
sub var  ($n) {  Var->new( ident => $n ) }

sub nil                 { List::Nil->new }
sub cons ($h, $t = nil) { List::Cons->new( head => $h, tail => $t ) }

sub app  ($var, $arg) { App->new( head => $var, tail => $arg ) }

sub closure ($abs, $env) { Abs::Closure->new( head => $abs, tail => $env ) }

sub lambda ($param, $body) { Abs::Lambda->new( head => $param, tail => $body ) }
sub native ($param, $body) { Abs::Native->new( head => $param, tail => $body ) }
sub fexpr  ($param, $body) {  Abs::FExpr->new( head => $param, tail => $body ) }



# ...

sub run ($expr, $env) {
    return evaluate( $expr, $env->derive );
}

sub evaluate ($expr, $env) {
    return $expr                 if $expr isa 'Num';
    return apply( $expr, $env )  if $expr isa 'App';
    return $env->lookup( $expr ) if $expr isa 'Var';
    return $expr                 if $expr isa 'List::Nil';
    if ($expr isa 'List::Cons') {
        my $head = evaluate( $expr->head, env );
        return cons( $head ) if ($expr->tail isa 'List::Nil');
        return cons( $head, evaluate( $expr->tail, env ) );
    }
    return Exception->throw('EVAL! Unknown Expr', $expr);
}

sub apply ($expr, $env) {
    my $abs = $expr->head;
       $abs = evaluate( $abs, $env ) unless $expr->head isa 'Abs';

    if ($abs isa 'Abs::FExpr') {
        return evaluate( $abs->tail->( $expr->tail, $env ), $env );
    } else {
        my $arg = evaluate( $expr->tail, $env );
        if ($abs isa 'Abs::Closure') {
            my $fun   = $abs->head;
            my $local = $abs->tail->derive( $fun->head->ident, $arg );
            return $fun->tail->( $local )         if $fun isa 'Abs::Native';
            return evaluate( $fun->tail, $local ) if $fun isa 'Abs::Lambda';
        } else {
            return closure( $abs->tail, $env->derive( $abs->head->ident, $arg ) );
        }
    }
    return Exception->throw('APPLY! Unknown Expr', $expr);
}

sub DEBUG ($label, $orig) {
    my $depth = 0;
    return sub ($expr, $env) {
        $depth++;
        my $indent = ('  ' x $depth);
        say sprintf '++ %-80s : %s' => (sprintf '%s%s > %s:%s' => $indent, $label, blessed $expr, $expr), $env;
        my $result = $orig->($expr, $env);
        say sprintf '-- %-80s : %s' => (sprintf '%s%s < %s:%s' => $indent, $label, blessed $result, $result), $env;
        $depth--;
        return $result;
    }
}

{
    no warnings 'redefine';
    *evaluate = DEBUG('EVAL ', \&evaluate);
    *apply    = DEBUG('APPLY', \&apply);
}

# ...

sub BINOP ($f, $c) {
    return fexpr(
        cons( var('args') ),
        sub ($args, $env) {
            app(
                app(
                    lambda( var('x'),
                        native( var('y'), sub ($e) {
                            my $lhs = $e->lookup( var('x') );
                            my $rhs = $e->lookup( var('y') );
                            $c->( $f->($lhs->value, $rhs->value) )
                        })
                    ),
                    $args->head
                ),
                $args->tail
            )
        }
    )
}

my $env = env(
    '==' => BINOP(sub ($n, $m) { $n == $m }, \&bool),
    '!=' => BINOP(sub ($n, $m) { $n != $m }, \&bool),
    '>'  => BINOP(sub ($n, $m) { $n >  $m }, \&bool),
    '>=' => BINOP(sub ($n, $m) { $n >= $m }, \&bool),
    '<'  => BINOP(sub ($n, $m) { $n <  $m }, \&bool),
    '<=' => BINOP(sub ($n, $m) { $n <= $m }, \&bool),
    '+'  => BINOP(sub ($n, $m) { $n + $m }, \&num),
    '-'  => BINOP(sub ($n, $m) { $n - $m }, \&num),
    '*'  => BINOP(sub ($n, $m) { $n * $m }, \&num),
    '/'  => BINOP(sub ($n, $m) { $n / $m }, \&num),
    '%'  => BINOP(sub ($n, $m) { $n % $m }, \&num),


    'my' => BINOP(sub ($n, $m) {

    }, \&id)
);


my $program = app( var('+'), cons( num(10), app( var('*'), cons( num(4), num(5) ) ) ) );

say "... ", $program;
say run( $program, $env );






