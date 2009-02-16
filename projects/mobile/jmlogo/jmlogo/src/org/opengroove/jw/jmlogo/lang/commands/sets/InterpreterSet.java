package org.opengroove.jw.jmlogo.lang.commands.sets;

public class InterpreterSet extends BaseCommandSet
{
    public static final InterpreterSet set = new InterpreterSet();
    
    protected void loadCommands()
    {
        addCommand(new NamedCommand("output", 1, 1)
        {
        });
    }
}
