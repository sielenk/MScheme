package MScheme.environment;

import MScheme.environment.*;
import MScheme.values.*;
import MScheme.code.*;
import MScheme.syntax.Syntax;
import MScheme.syntax.SyntaxFactory;
import MScheme.exceptions.*;


public class TestEnvironment
    extends junit.framework.TestCase
{
    protected DynamicEnvironment env;
    protected Symbol sym1;
    protected Symbol sym2;
    protected Value  val1;
    protected Value  val2;
    
    public TestEnvironment(String name)
    { super(name); }
    
    protected void setUp()
    {
        env = DynamicEnvironment.getEmpty();
    
        sym1 = ValueFactory.createSymbol("test1");
        sym2 = ValueFactory.createSymbol("test2");
    
        val1 = ValueFactory.createList();
        val2 = ValueFactory.createTrue();
    }
    
    protected void tearDown()
    { 
        env = null;
        sym1 = sym2 = null;
        val1 = val2 = null;
    }
    

    public void testTestPattern()
    {
        assert("different symbols are equal (==)", sym1 != sym2);
        assert("different symbols are equal (equals)", !sym1.equals(sym2));
        assert("different entities are equal (==)", val1 != val2);
        assert("different entities are equals (equals)", !val1.equals(val2));
    }
    
    public void testNormal()
        throws Exception
    {
        try {
            env.lookup(sym1);
            fail("env not empty");
        }
        catch (SymbolNotFoundException e) { }
    
        try {
            env.assign(sym1, val1);
            fail("expected SymbolNotFound exception");
        }
        catch (SymbolNotFoundException e) { }
        
        env.define(sym1, val1);
        
        assert(
            "lookup failed",
            env.lookup(sym1) == val1
        );
        
        env.assign(sym1, val2);
        
        assert(
            "assign failed",
            env.lookup(sym1) == val2
        );
    }

    public void testChild()
        throws Exception
    {
        DynamicEnvironment child  = env.newChild();
        
        assert(child != env);
        
        env  .define(sym1, val1);
        child.define(sym2, val2);
        
        assert(child.lookup(sym1) == val1);
        assert(child.lookup(sym2) == val2);
        assert(env  .lookup(sym1) == val1);
        
        try {
            env.lookup(sym2);
            fail("expected SymbolNotFoundException");
        }
        catch (SymbolNotFoundException e) { }
        
        env.define(sym2, val1);
        assert(child.lookup(sym2) == val2);
        assert(env  .lookup(sym2) == val1);
    }
    
    public void testSyntax()
        throws Exception
    {
        StaticEnvironment env = new StaticEnvironment();
        
        try {
            env.getTokenFor(sym1);
            fail("expected SymbolNotFoundException");
        }
        catch (SymbolNotFoundException e) { }
        
        try {
            env.getTokenFor(sym1);
            fail("expected SymbolNotFoundException");
        }
        catch (SymbolNotFoundException e) { }
        
        Syntax    token = SyntaxFactory.getBeginToken();
        env.defineSyntax(sym1, token);
        
        assert(env.getTokenFor(sym1) == token);
        
        try {
            env.getCodeFor(sym1);
            fail("expected UnexpectedSyntax");
        }
        catch (UnexpectedSyntax e) { }

        Reference reference = env.define(sym2);

        assert(env.getTokenFor(sym2) == reference);
        assert(env.getCodeFor (sym2) == reference);
    }
    
    public void testExtendedStatic()
        throws Exception
    {
        env.getStatic().define(sym1);

        try {       
            env.lookup(sym1);
            fail("expected UninitializedSymbolException");
        }
        catch (UninitializedSymbolException e) { }
        
        env.assign(sym1, val1);
        
        assert(
            env.lookup(sym1) == val1
        );
    }
}

