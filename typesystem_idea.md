# Good Error Report Type System

## why I hate ML type system

    We have hindley-milner type system for  polymorphic languages like ML.
    but the type does not contain where the type was born.
    therefore it seems that we can't precisely report where type error was born.
    if we ask Unify to calculate type maybe unify fail and raise failed typing. but type does not contain span therefore we have to dig vary large chunk of code to find type errors.
    if there is no type error we can remove Span information from Type.
    but this information willbe very useful for Language Servers.

## promoting type system

my type system contains the span Information for all basic types functional types. in the following grammer.

$$
    \tau ::= \iota
            | \tau \to \tau
            | \tau * \tau
            | t
            | spanned(\tau)
$$

Therefore if we raise error from Unify they contains span information.
