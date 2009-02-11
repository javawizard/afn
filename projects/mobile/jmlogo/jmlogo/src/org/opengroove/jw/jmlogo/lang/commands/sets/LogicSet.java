package org.opengroove.jw.jmlogo.lang.commands.sets;

import org.opengroove.jw.jmlogo.lang.Command;
import org.opengroove.jw.jmlogo.lang.InterpreterContext;
import org.opengroove.jw.jmlogo.lang.NamedCommand;
import org.opengroove.jw.jmlogo.lang.Token;
import org.opengroove.jw.jmlogo.lang.WordToken;

public class LogicSet
{
    public static final Command[] set = new Command[] { new NamedCommand("not", 1, 1)
    {
        
        public Token run(InterpreterContext context, Token[] arguments)
        {
            validateWord(arguments[0]);
            WordToken word = (WordToken) arguments[0];
            return new WordToken(!word.getBool());
        }
    } };
}
