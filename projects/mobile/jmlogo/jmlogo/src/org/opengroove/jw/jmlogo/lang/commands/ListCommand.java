package org.opengroove.jw.jmlogo.lang.commands;

import org.opengroove.jw.jmlogo.lang.NamedCommand;
/**
 * A command that wraps it's arguments into a list.
 * @author Alexander Boyd
 *
 */
public class ListCommand extends NamedCommand
{
    
    protected ListCommand()
    {
        super("list", 2, 256);
    }
    
}
