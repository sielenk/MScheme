package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.machine.Literal;
import MScheme.code.Code;
import MScheme.environment.StaticEnvironment;
import MScheme.values.*;

import MScheme.exceptions.*;


// *** quote ***

final class QuasiquoteToken
    extends Syntax
{
    final static Syntax INSTANCE = new QuasiquoteToken();
    
    private QuasiquoteToken()
    { super(Arity.exactly(1)); }
    
    protected Code checkedTranslate(
        StaticEnvironment syntax,
	    int               len,
        List              arguments
    ) throws CompileError, TypeError
    { return qqExpand(arguments.getHead(), 0).getCode(syntax); }


    static Symbol unquote          = Symbol.create("unquote");
    static Symbol unquote_splicing = Symbol.create("unquote-splicing");
    static Symbol quasiquote       = Symbol.create("quasiquote");

    Value qqExpand(Value value, int level)
        throws CompileError, TypeError
    {
        if (value.isPair()) {
            Pair  pair = value.toPair();
            Value fst  = pair.getFirst();
            
            if (isUnquote(fst)) {
                if (level == 0) {
                    return pair.getTail().getHead();
                } else {
                    return ValueFactory.createList(
                        unquote,
                        qqExpand(pair.getTail().getHead(), level - 1)
                    );
                }
            } else if (isUnquoteSplicing(fst)) {
                throw new CompileError(value);
            } else if (isQuasiquote(fst)) {
                return ValueFactory.createList(
                    quasiquote,
                    qqExpand(pair.getTail().getHead(), level + 1)
                );
            }

            return ValueFactory.createList(
                Symbol.create("append"),
                qqExpandList(fst             , level),
                qqExpand    (pair.getSecond(), level)
            );
        } else if (value.isScmVector()) {
            return ValueFactory.createList(
                Symbol.create("list->vector"),
                qqExpand(value.toScmVector().getList().toValue(), level)
            );
        }

        return ValueFactory.createList(
            Symbol.create("quote"),
            value
        );
    }

    Value qqExpandList(Value value, int level)
        throws CompileError, TypeError
    {
        if (value.isPair()) {
            Pair  pair = value.toPair();
            Value fst  = pair.getFirst();
            
            if (isUnquote(fst)) {
                if (level == 0) {
                    return ValueFactory.createList(
                        Symbol.create("list"),
                        pair.getTail().getHead()
                    );
                } else {
                    return ValueFactory.createList(
                        Symbol.create("list"),
                        ValueFactory.createList(
                            unquote,
                            qqExpand(pair.getTail().getHead(), level - 1)
                        )
                    );
                }
            } else if (isUnquoteSplicing(fst)) {
                if (level == 0) {
                    return pair.getTail().getHead();
                } else {
                    return ValueFactory.createList(
                        Symbol.create("list"),
                        ValueFactory.createList(
                            unquote_splicing,
                            qqExpand(pair.getTail().getHead(), level - 1)
                        )
                    );
                }
            } else if (isQuasiquote(fst)) {
                return ValueFactory.createList(
                    Symbol.create("list"),
                    ValueFactory.createList(
                        quasiquote,
                        qqExpand(pair.getTail().getHead(), level + 1)
                    )
                );
            }

            return ValueFactory.createList(
                Symbol.create("list"),
                ValueFactory.createList(
                    Symbol.create("append"),
                    qqExpandList(fst             , level),
                    qqExpand    (pair.getSecond(), level)
                )
            );
        }

        return ValueFactory.createList(
            Symbol.create("quote"),
            ValueFactory.createList(
                value
            )
        );
    }

    boolean isUnquote(Value v)
    { return v.eq(unquote); }

    boolean isUnquoteSplicing(Value v)
    { return v.eq(unquote_splicing); }

    boolean isQuasiquote(Value v)
    { return v.eq(quasiquote); }
}
  
