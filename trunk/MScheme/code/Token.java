package MScheme.code;

import MScheme.environment.StaticEnvironment;
import MScheme.values.List;
import MScheme.exceptions.SchemeException;


public abstract class Token
{
    public abstract Code translateArguments(
        StaticEnvironment e,
        List              arguments
    ) throws SchemeException;
}
