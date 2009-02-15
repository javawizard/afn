package org.opengroove.jw.jmlogo.lang.commands.sets;

import org.opengroove.jw.jmlogo.lang.InterpreterContext;
import org.opengroove.jw.jmlogo.lang.InterpreterException;
import org.opengroove.jw.jmlogo.lang.ListToken;
import org.opengroove.jw.jmlogo.lang.NamedCommand;
import org.opengroove.jw.jmlogo.lang.Token;
import org.opengroove.jw.jmlogo.lang.WordToken;

public class DataProcessingSet2 extends BaseCommandSet
{
    public static DataProcessingSet2 set = new DataProcessingSet2();
    
    protected void loadCommands()
    {
        addCommand(new NamedCommand("first", 1, 1)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                if (arguments[0] instanceof WordToken)
                {
                    WordToken w = (WordToken) arguments[0];
                    if (w.getValue().length() < 1)
                        throw new InterpreterException(
                            "The word specified is empty, so you "
                                + "can't run the first command on it");
                    return new WordToken(w.getValue().substring(0, 1));
                }
                else
                {
                    ListToken t = (ListToken) arguments[0];
                    if (t.getMembers().length < 1)
                        throw new InterpreterException(
                            "The list specified is empty, so you "
                                + "can't run the first command on it");
                    return t.getMembers()[0];
                }
            }
        });
        addCommand(new NamedCommand("butfirst", 1, 1)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                if (arguments[0] instanceof WordToken)
                {
                    WordToken t = (WordToken) arguments[0];
                    if (t.getValue().length() < 1)
                        throw new InterpreterException(
                            "empty word specified to butfirst");
                    return new WordToken(t.getValue().substring(1));
                }
                else
                {
                    ListToken t = (ListToken) arguments[0];
                    return t.butFirst();
                }
            }
        });
        addCommand(new NamedCommand("count", 1, 1)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                if (arguments[0] instanceof WordToken)
                {
                    WordToken t = (WordToken) arguments[0];
                    return new WordToken(t.getValue().length());
                }
                else
                {
                    ListToken t = (ListToken) arguments[0];
                    return new WordToken(t.getMembers().length);
                }
            }
        });
        addCommand(new NamedCommand("thing", 1, 1)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                verifyWord(arguments[0]);
                WordToken w = (WordToken) arguments[0];
                Token v = context.getVariable(w.getValue());
                if (v == null)
                    throw new InterpreterException("" + w.getValue()
                        + " has no value, passed to the thing command");
                return v;
            }
        });
        addCommand(new NamedCommand("ern", 1, 1000)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                verifyWords(arguments, 0, arguments.length);
                for (int i = 0; i < arguments.length; i++)
                {
                    context.eraseVariable(((WordToken) arguments[i]).getValue());
                }
                return null;
            }
            
        });
    }
}
