package org.opengroove.jw.jmlogo.lang;

/**
 * A logo command. Each primitive command is implemented as a subclass of
 * Command.
 * 
 * @author Alexander Boyd
 * 
 */
public abstract class Command
{
    /**
     * Returns the number of arguments that this command takes. Support for
     * variable argument sizes is not present yet; use a list instead.
     * 
     * @return
     */
    public abstract int getArgumentCount();
    
    /**
     * Returns this command's name as logo programs should use to call the
     * command.
     * 
     * @return
     */
    public abstract String getName();
    
    /**
     * Actually runs this command.
     * 
     * @param context
     */
    public abstract Token run(InterpreterContext context, Token[] arguments);
    
    protected void validateWord(Token token)
    {
        if (!(token instanceof WordToken))
            throw new InterpreterException("Token was not a word");
    }
    
    protected void validateList(Token token)
    {
        if (!(token instanceof ListToken))
            throw new InterpreterException("Token was not a list");
    }
}
