package MScheme;

import MScheme.values.*;
import MScheme.environment.*;
import MScheme.code.*;
import MScheme.machine.Machine;

public class Main
{
    public final static void main(String[] argv)
        throws Exception
    {
        InputPort  in  = ValueFactory.createInputPort();
        OutputPort out = ValueFactory.createOutputPort();

        DynamicEnvironment environment
            = DynamicEnvironment.getEmpty();
    
        StaticEnvironment staticBindings
            = environment.getStatic();
        
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("quote"),
            SyntaxFactory.getQuoteToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("if"),
            SyntaxFactory.getIfToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("begin"),
            SyntaxFactory.getBeginToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("lambda"),
            SyntaxFactory.getLambdaToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("define"),
            SyntaxFactory.getDefineToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("set!"),
            SyntaxFactory.getSetToken()
        );

        Machine machine = new Machine(environment);
        
        out.write(machine.evaluate(in.read()));
        
        out.writeChar('\n');
    }
}
