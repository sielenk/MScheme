package MScheme;

import MScheme.Code;
import MScheme.values.List;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.TypeError;
import MScheme.exceptions.CompileError;


public interface Token
{
    Code translate(
        StaticEnvironment e,
        List              arguments
    ) throws CompileError, TypeError;
}
