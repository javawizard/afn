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
        
    }
}
