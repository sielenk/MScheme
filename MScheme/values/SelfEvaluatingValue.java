package MScheme.values;

import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.machine.Literal;


abstract class SelfEvaluatingValue
    extends Value
{
    // specialisation of Value
    
    final public Code getCode(StaticEnvironment e)
    { return new Literal(this); }
}

