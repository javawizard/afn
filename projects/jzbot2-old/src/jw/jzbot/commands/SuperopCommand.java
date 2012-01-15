package jw.jzbot.commands;

import java.util.ArrayList;

import net.sf.opengroove.common.utils.StringUtils;

import jw.jzbot.Command;
import jw.jzbot.JZBot;
import jw.jzbot.ResponseException;
import jw.jzbot.fact.functions.HashFunction;
import jw.jzbot.scope.Messenger;
import jw.jzbot.scope.UserMessenger;
import jw.jzbot.storage.Operator;
import jw.jzbot.storage.Server;
import jw.jzbot.utils.Utils;

public class SuperopCommand implements Command
{
    
    public String getName()
    {
        return "superop";
    }
    
    public void run(String server, String channel, boolean pm, UserMessenger sender,
            Messenger source, String arguments)
    {
        if (server == null)
            throw new ResponseException(
                    "The superop command requires a server to execute under.");
        // TODO: consider having "founders", which are like superops but can't remove each
        // other, and can't be removed by superops; they should probably be declared in a
        // server-side file.
        sender.verifySuperop();
        String[] tokens = arguments.split(" ", 2);
        String subcommand = tokens[0];
        if (subcommand.equals("list"))
        {
            Server dServer = JZBot.storage.getServer(server);
            ArrayList<String> strings = new ArrayList<String>();
            for (Operator op : dServer.getOperators().isolate())
            {
                strings.add(op.getHostname());
            }
            Utils.ircSendDelimited(strings.toArray(new String[0]), "  ", source);
            source
                    .sendMessage("End of superop list. These superops are the superops at "
                        + "this server; this list does not include superops at "
                        + "other servers.");
        }
        else if (subcommand.equals("add"))
        {
            if (tokens.length == 0 || tokens.length == 1)
            {
                source.sendMessage("You need to specify a hostname.");
                return;
            }
            String newHostname = tokens[1];
            if (newHostname.trim().equals(""))
                throw new ResponseException(
                        "You can't add the empty hostmask as a superop.");
            Operator op = JZBot.storage.createOperator();
            op.setHostname(newHostname);
            Server dServer = JZBot.storage.getServer(server);
            dServer.getOperators().add(op);
            source.sendMessage("Hostname " + newHostname
                + " was successfully added as a superop on " + server + ".");
        }
        else if (subcommand.equals("delete"))
        {
            if (tokens.length == 0)
            {
                source.sendMessage("You need to specify a hostname.");
                return;
            }
            String newHostname = tokens[1];
            Server dServer = JZBot.storage.getServer(server);
            Operator op = dServer.getOperator(newHostname);
            if (op == null)
            {
                source.sendMessage("That hostname isn't a superop.");
                return;
            }
            dServer.getOperators().remove(op);
            source.sendMessage("Removed.");
        }
        else if (subcommand.equals("key"))
        {
            throw new RuntimeException("Due to some recent changes, keys "
                + "are currently disabled. I might add this concept "
                + "back in the future. If someone who knows Java wants "
                + "to fix this up, contact the project owners at "
                + "http://jzbot.googlecode.com");
            // String theKey = tokens[1];
            // String theHash = HashFunction.doHash(theKey);
            // if (StringUtils.isMemberOf(theHash, ConfigVars.keys.get().split("\\|")))
            // {
            // JZBot.elevate(sender.getServerName(), sender.getHostname(),
            // ConfigVars.primary.get());
            // source.sendMessage("That key is correct. Your hostname has now been "
            // + "added as a superop. This will persist until the "
            // + "bot is restarted.");
            // return;
            // }
            // else
            // {
            // throw new ResponseException("Incorrect key.");
            // }
        }
        
        else
        {
            source.sendMessage("Specify one of add, list, key, or delete.");
            return;
        }
        
    }
    
    @Override
    public boolean relevant(String server, String channel, boolean pm, UserMessenger sender,
            Messenger source, String arguments)
    {
        return true;
    }
}
