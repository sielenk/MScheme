package MScheme.values;

import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;

import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.exceptions.*;


public abstract class Value
{
    /** 
     */
    public void setLiteral() { }

    /** <code>true</code> only for the false singleton */
    public boolean isFalse() { return false; }
    
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
    public boolean isChar()     { return false; }
    public boolean isString()   { return false; }
    public boolean isVector()   { return false; }
    public boolean isPort()     { return false; }
    public boolean isFunction() { return false; }


    // type casts (to get SchemeExceptions instead of ClassCastExceptions)
    
    public List         toList     () throws     ListExpectedException
    { throw new ListExpectedException(this); }

    public Pair         toPair     () throws     PairExpectedException
    { throw new PairExpectedException(this); }

    public Symbol       toSymbol   () throws   SymbolExpectedException
    { throw new SymbolExpectedException(this); }

    public SchemeNumber toNumber   () throws   NumberExpectedException
    { throw new NumberExpectedException(this); }

    public SchemeChar   toChar     () throws     CharExpectedException
    { throw new CharExpectedException(this); }

    public SchemeString toScmString() throws   StringExpectedException
    { throw new StringExpectedException(this); }

    public SchemeVector toVector   () throws   VectorExpectedException
    { throw new VectorExpectedException(this); }

    public Port         toPort     () throws     PortExpectedException
    { throw new PortExpectedException(this); }

    public Function     toFunction () throws FunctionExpectedException
    { throw new FunctionExpectedException(this); }


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
    
    public abstract Code getCode(StaticEnvironment e)
        throws SchemeException;
}

