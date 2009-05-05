package org.opengroove.jzbot.plugins;

import java.net.URI;
import java.util.ArrayList;

import org.opengroove.jzbot.JZBot;

public class CommandInvocationContext
{
    /**
     * Messages which are to be sent to the source of the command invocation.
     * These are the replies to the command. If a command is called in single
     * capture mode, then only one message can be inserted here, or the call
     * will throw an exception when it returns.
     */
    private ArrayList<Message> toSourceMessages = new ArrayList<Message>();
    /**
     * Messages which are "other" messages for this command. These are messages
     * that are not considered a reply to the command, and are typically not
     * sent to the message sender. For example, when "~roulette show" is run,
     * the roulette command adds the
     * "so-and-so has seen which chamber is loaded" message as a to-source
     * message, and "chamber X is loaded" as an other message directly to the
     * user.
     * 
     * Capture mode cannot usually capture these messages. Instead, they are
     * added to the parent command invocation context's other message list when
     * a subcommand returns. In contrast, to-source messages can be captured.
     */
    private ArrayList<TargetedMessage> otherMessages = new ArrayList<TargetedMessage>();
    /**
     * True if this context is finished, false if it is not. A context passed
     * into a command will never be finished; contexts are only finished by
     * jzbot itself. Most methods throw IllegalStateExceptions if they are
     * called when the context has been finished.<br/>
     * <br/>
     * 
     * A context enters into the finished state when the finish method is called
     * on it. This method essentially forwards all messages to the parent
     * context if this context is not in capture mode.
     */
    private boolean finished;
    /**
     * The parent command invocation context. If this invocation context is not
     * in capture mode, then calling finish() will cause to-source messages and
     * other messages to be added to the parent context. If this invocation
     * context is in capture mode, then calling finish() will cause only other
     * messages to be added to the parent context.
     */
    private CommandInvocationContext parent;
    /**
     * True if this context is in capture mode. In capture mode, to-source
     * messages are not sent to the parent context after the command runs, but
     * are instead captured and made available to the command invoker. Other
     * messages are still sent to the parent.
     */
    private boolean captureMode;
    
    private URI source;
    private URI user;
    
    /**
     * Creates a command invocation context that is a child of the specified
     * context.
     * 
     * @param parent
     *            The parent context
     * @param captureMode
     *            True if this context is to be created in {@link #captureMode},
     *            false if it is to be created in normal mode
     */
    public CommandInvocationContext(CommandInvocationContext parent,
        boolean captureMode, URI source, URI user)
    {
        this.parent = parent;
        this.captureMode = captureMode;
        this.source = source;
        this.user = user;
    }
    
    /**
     * Creates a context without a parent. This is what is called by JZBot for
     * commands that are called as a result of someone directly sending the
     * command in a message. Contexts without a parent are never created in
     * capture mode.
     */
    public CommandInvocationContext(URI source, URI user)
    {
        this(null, false, source, user);
    }
    
    public void sendToSource(boolean action, String message)
    {
        sendToSource(new Message(action, message));
    }
    
    public void sendToSource(String message)
    {
        sendToSource(false, message);
    }
    
    public void sendToSource(Message message)
    {
        checkNotFinished();
        toSourceMessages.add(message);
    }
    
    public void sendOther(URI target, boolean action, String message)
    {
        sendOther(new TargetedMessage(target, action, message));
    }
    
    public void sendOther(URI target, String message)
    {
        sendOther(target, false, message);
    }
    
    public void sendOther(TargetedMessage message)
    {
        checkNotFinished();
        otherMessages.add(message);
    }
    
    private void checkNotFinished()
    {
        if (finished)
            throw new IllegalStateException("Context is already finished");
    }
    
    private void checkFinished()
    {
        if (!finished)
            throw new IllegalStateException("Context is not finished");
    }
    
    public void finish()
    {
        checkNotFinished();
        if (parent != null)
        {
            for (Message m : toSourceMessages)
            {
                parent.sendToSource(m);
            }
            if (!captureMode)
            {
                for (TargetedMessage m : otherMessages)
                {
                    parent.sendOther(m);
                }
            }
        }
        finished = true;
    }
    
    public void checkNoParent()
    {
        if (parent != null)
            throw new IllegalStateException("Context has a parent");
    }
    
    public void checkParent()
    {
        if (parent == null)
            throw new IllegalStateException("Context doesn't have a parent");
    }
    
    public Message[] getCapturedMessages()
    {
        checkFinished();
        checkParent();
        return toSourceMessages.toArray(new Message[0]);
    }
    
    public Message[] getToSourceMessages()
    {
        checkFinished();
        checkNoParent();
        return toSourceMessages.toArray(new Message[0]);
    }
    
    public TargetedMessage[] getOtherMessages()
    {
        checkFinished();
        checkNoParent();
        return otherMessages.toArray(new TargetedMessage[0]);
    }
    
    public Message invokeSingleCapture(String command, String arguments)
    {
        return invokeSingleCapture(command, arguments, this.source);
    }
    
    public Message invokeSingleCapture(String command, String arguments, URI source)
    {
        CommandInvocationContext subcontext =
            new CommandInvocationContext(this, true, source, this.user);
        JZBot
            .executeCommandToContext(subcontext, command, arguments, this.user, source);
        subcontext.finish();
        Message[] capturedMessages = subcontext.getCapturedMessages();
        if (capturedMessages.length > 1)
            throw new ExcessiveResponseException("Command " + command
                + " called in single capture mode but returned "
                + capturedMessages.length + " to-source messages");
        if (capturedMessages.length == 0)
            return null;
        return capturedMessages[0];
    }
}
