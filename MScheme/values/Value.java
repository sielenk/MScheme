package MScheme.values;

import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;

import MScheme.environment.DynamicEnvironment;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Token;
import MScheme.code.Code;
import MScheme.machine.Literal;
import MScheme.exceptions.*;


public abstract class Value
{
    /** 
     */
    public void setConst() { }

    /** <code>false</code> only for the false singleton */
    public boolean isTrue() { return true; }

    /** true for the empty list,
     *  called recursively by pairs on their second element,
     *  false otherwise.
     */
    public boolean isList() { return false; }

    
    // disjoint types (as in R5RS, 3.2)
        
    public boolean isBoolean()  { return false; }
    public boolean isPair()     { return false; }
    public boolean isSymbol()   { return false; }
    public boolean isNumber()   { return false; }
    public boolean isScmChar()  { return false; }
    public boolean isString()   { return false; }
    public boolean isVector()   { return false; }
    public boolean isPort()     { return false; }
    public boolean isFunction() { return false; }


    // type casts (to get SchemeExceptions instead of ClassCastExceptions)
    
    public List         toList     () throws     ListExpected
    { throw new ListExpected(this); }

    public Pair         toPair     () throws     PairExpected
    { throw new PairExpected(this); }

    public Symbol       toSymbol   () throws   SymbolExpected
    { throw new SymbolExpected(this); }

    public SchemeNumber toNumber   () throws   NumberExpected
    { throw new NumberExpected(this); }

    public ScmChar      toScmChar  () throws     CharExpected
    { throw new CharExpected(this); }

    public SchemeString toScmString() throws   StringExpected
    { throw new StringExpected(this); }

    public SchemeVector toVector   () throws   VectorExpected
    { throw new VectorExpected(this); }

    public Port         toPort     () throws     PortExpected
    { throw new PortExpected(this); }

    public Function     toFunction () throws FunctionExpected
    { throw new FunctionExpected(this); }

    public DynamicEnvironment toEnvironment() throws EnvironmentExpected
    { throw new EnvironmentExpected(this); }


    // equivalence predicates (as in R5RS, 6.1)
    
    public boolean eq(Value other)
    { return this == other; }
    
    public boolean eqv(Value other)
    { return eq(other); }
    
    public boolean equal(Value other)
    { return eqv(other); }

    public final boolean equals(Object other)
    {
        if (!(other instanceof Value)) {
            return false;
        }
    
        return equal((Value)other);
    }
    
    
    // io functions
    
    public abstract void write(Writer destination)
        throws IOException;
    
    public void display(Writer destination)
        throws IOException
    { write(destination); }


    public final String toString()
    {
        StringWriter out = new StringWriter();

        try { write(out); }
        catch (IOException e) { }

        return out.toString();
    }


    // compilation functions

    public final Literal getLiteral()
    { return new Literal(this); }
    
    public Code getCode(StaticEnvironment e)
        throws CompileError, TypeError
    { return getLiteral(); }

    public Token getToken(StaticEnvironment e)
        throws CompileError, TypeError
    { return getCode(e); }
}
