package org.opengroove.jw.jmlogo.lang.commands.sets;

import org.opengroove.jw.jmlogo.lang.Command;
import org.opengroove.jw.jmlogo.lang.InterpreterContext;
import org.opengroove.jw.jmlogo.lang.ListToken;
import org.opengroove.jw.jmlogo.lang.NamedCommand;
import org.opengroove.jw.jmlogo.lang.Token;

public class DataProcessingSet
{
    public static final Command[] set =
        new Command[] { new NamedCommand("list", 2, 256)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                return new ListToken(arguments);
            }
        }, new NamedCommand("wraplist", 1, 1)
        {

            public Token run(InterpreterContext context, Token[] arguments)
            {
                return new ListToken(arguments);
            }
        } };
}
