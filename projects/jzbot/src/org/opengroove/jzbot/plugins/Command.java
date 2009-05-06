package org.opengroove.jzbot.plugins;

import java.net.URI;

public interface Command
{
    /**
     * Initializes this command. This will only be called once.
     */
    public void init();
    
    /**
     * Returns true if the specified line sent to jzbot can be processed by this
     * command.
     * 
     * @param command
     *            The command. This is the first word on the line.
     * @param arguments
     *            The command's arguments. This is the rest of the line after
     *            the command, or the empty string if only the command was
     *            given.
     * @param source
     *            The source of the command. This is a room if the command was
     *            sent to a room, or <tt>user</tt> if the command was sent in a
     *            pm.
     * @param user
     *            The user that sent this command
     * @return True if this command class can process the specified line, false
     *         if it cannot
     */
    public boolean canProcess(String command, String arguments, URI source, URI user);
    
    /**
     * Processes the specified line, calling back to JZBot to send any response
     * that might be needed.
     * 
     * @param command
     *            The command
     * @param arguments
     *            The arguments to the command
     * @param context
     *            The context of the command. This can be used to call other
     *            commands, and it can be used to retrieve the source and user.
     */
    public void process(String command, String arguments,
        CommandInvocationContext context);
}
