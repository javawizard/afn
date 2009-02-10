package org.opengroove.jw.jmlogo.lang;

public class NamedCommand extends Command
{
    private String name;
    private int minArgs;
    private int maxArgs;
    
    protected NamedCommand(String name, int minArgs, int maxArgs)
    {
        
    }
    
    public int getArgumentCount()
    {
        // TODO Auto-generated method stub
        return 0;
    }
    
    public String getName()
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public Token run(InterpreterContext context, Token[] arguments)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
}
