package MScheme;

import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;

import MScheme.values.*;
import MScheme.code.Literal;

import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;
import MScheme.exceptions.*;


public abstract class Value
{
    public final static String id
        = "$Id$";

    public void setConst() { }

    public boolean isTrue() { return true; }

    public boolean isList() { return false; }

    
    // disjoint types (as in R5RS, 3.2)
        
    public boolean isScmBoolean() { return false; }
    public boolean isPair()       { return false; }
    public boolean isSymbol()     { return false; }
    public boolean isScmNumber()  { return false; }
    public boolean isScmChar()    { return false; }
    public boolean isScmString()  { return false; }
    public boolean isScmVector()  { return false; }
    public boolean isPort()       { return false; }
    public boolean isFunction()   { return false; }


    // type casts (to get SchemeExceptions instead of ClassCastExceptions)
    
    public List toList()
        throws     ListExpected
    { throw new ListExpected(this); }

    public Pair toPair()
        throws PairExpected
    { throw new PairExpected(this); }

    public Symbol toSymbol()
        throws SymbolExpected
    { throw new SymbolExpected(this); }

    public ScmNumber toScmNumber()
        throws NumberExpected
    { throw new NumberExpected(this); }

    public ScmChar toScmChar()
        throws CharExpected
    { throw new CharExpected(this); }

    public ScmString toScmString()
        throws StringExpected
    { throw new StringExpected(this); }

    public ScmVector toScmVector()
        throws VectorExpected
    { throw new VectorExpected(this); }

    public InputPort toInputPort()
        throws InputPortExpected
    { throw new InputPortExpected(this); }

    public OutputPort toOutputPort()
        throws OutputPortExpected
    { throw new OutputPortExpected(this); }

    public Function toFunction()
        throws FunctionExpected
    { throw new FunctionExpected(this); }

    public Environment toEnvironment()
        throws EnvironmentExpected
    { throw new EnvironmentExpected(this); }

    public StaticEnvironment toStaticEnvironment()
        throws EnvironmentExpected
    { throw new EnvironmentExpected(this); }


    // scheme equivalence predicates (as in R5RS, 6.1)
    
    public boolean eq(Value other)
    { return this == other; }
    
    public boolean eqv(Value other)
    { return eq(other); }
    
    public boolean equal(Value other)
    { return eqv(other); }

    
    // the java equivalence predicate

    public final boolean equals(Object other)
    {
        if (!(other instanceof Value)) {
            return false;
        }
    
        return equal((Value)other);
    }
    
    
    // scheme io functions
    
    public abstract void write(Writer destination)
        throws IOException;
    
    public void display(Writer destination)
        throws IOException
    { write(destination); }


    // the java io function

    public final String toString()
    {
        StringWriter out = new StringWriter();

        try { write(out); }
        catch (IOException e) { }

        return out.toString();
    }


    // compilation functions

    public final Literal getLiteral()
    { return Literal.create(this); }

    public MScheme.Code getCode(StaticEnvironment e)
        throws CompileError, TypeError
    { return getLiteral(); }

    public Translator getTranslator(StaticEnvironment e)
        throws CompileError, TypeError
    { return getCode(e); }
}
