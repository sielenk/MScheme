package MScheme.values;

import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.code.Quotation;


abstract class SelfEvaluatingValue
    extends Value
{
    // specialisation of Value
    
    final public Code getCode(StaticEnvironment e)
    { return new Quotation(this); }
}

