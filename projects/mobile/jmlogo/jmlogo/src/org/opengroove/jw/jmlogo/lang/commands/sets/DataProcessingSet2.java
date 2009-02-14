package org.opengroove.jw.jmlogo.lang.commands.sets;

public class DataProcessingSet2 extends BaseCommandSet
{
    public static DataProcessingSet2 set = new DataProcessingSet2();
    
    protected void loadCommands()
    {
        addCommand(new NamedCommand("first", 1, 1){}, new NamedCommand("last", 1, 1), new NamedCommand(""))
    }
}
