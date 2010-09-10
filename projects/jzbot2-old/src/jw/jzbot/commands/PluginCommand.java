package jw.jzbot.commands;

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.TreeSet;

import net.sf.opengroove.common.utils.StringUtils;

import jw.jzbot.Command;
import jw.jzbot.FactScope;
import jw.jzbot.JZBot;
import jw.jzbot.ResponseException;
import jw.jzbot.plugins.Plugin;
import jw.jzbot.plugins.PluginSystem;
import jw.jzbot.scope.Messenger;
import jw.jzbot.scope.UserMessenger;
import jw.jzbot.storage.PersistentKey;
import jw.jzbot.pastebin.PastebinUtils;

public class PluginCommand implements Command
{
    
    @Override
    public String getName()
    {
        return "plugin";
    }
    
    @Override
    public void run(String server, String channel, boolean pm, UserMessenger sender,
            Messenger source, String arguments)
    {
        synchronized (PluginSystem.class)
        {
            String[] argumentsTokenized1 = arguments.split(" ", 2);
            String command = argumentsTokenized1[0];
            String afterCommand =
                    (argumentsTokenized1.length > 1) ? argumentsTokenized1[1] : "";
            if (command.equals("list"))
            {
                String response = "";
                TreeSet<String> list = new TreeSet<String>();
                list.addAll(PluginSystem.enabledPluginNames);
                list.addAll(PluginSystem.failedPluginNames);
                list.addAll(PluginSystem.knownPluginNames);
                list.addAll(PluginSystem.loadedPluginNames);
                ArrayList<String> resultList = new ArrayList<String>();
                for (String name : list)
                {
                    String flags = "";
                    if (PluginSystem.knownPluginNames.contains(name))
                        flags += "1";
                    if (PluginSystem.enabledPluginNames.contains(name))
                        flags += "2";
                    if (PluginSystem.loadedPluginNames.contains(name))
                        flags += "3";
                    if (PluginSystem.failedPluginNames.contains(name))
                        flags += "4";
                    resultList.add(flags + ":" + name);
                }
                response += StringUtils.delimited(resultList, " ");
                if (response.equals(""))
                    response = "No available plugins.";
                else
                    source.sendSpaced("Here's the list. Plugins in "
                        + "this list are in the format <flags>:<name>, where "
                        + "<flags> are some flags and <name> is the name of "
                        + "the plugin. Flags are 1: installed (or included "
                        + "with JZBot), 2: enabled, 3: active (meaning the "
                        + "plugin is currently loaded and running), 4: error "
                        + "occurred while loading this plugin the last time "
                        + "the bot was started.");
                if (response.length() > (source.getProtocolDelimitedLength() * 2))
                    response = PastebinUtils.pastebinNotice(response, null);
                source.sendSpaced(response);
            }
            else if (command.equals("enable"))
            {
                String name = afterCommand;
                if (name.equals(""))
                    throw new ResponseException("You need to specify the name "
                        + "of the plugin to enable, like \"plugin enable "
                        + "some-example-plugin\". This plugin must be in "
                        + "the list of available plugins, so its language "
                        + "support plugin, if any, must be activated first.");
                if (PluginSystem.enabledPluginNames.contains(name))
                    throw new ResponseException("That plugin is already "
                        + "enabled. That doesn't mean it's active; try "
                        + "\"plugin active\" for a list of active plugins. "
                        + "If it's enabled but not active, you might need "
                        + "to restart the bot first, or it may have "
                        + "encountered an error when loading on the last bot restart.");
                if (!PluginSystem.knownPluginNames.contains(name))
                    throw new ResponseException("That plugin is not installed, or "
                        + "its language support plugin is not currently active. "
                        + "You need to install that plugin or activate its "
                        + "language support plugin before you can enabled it.");
                PluginSystem.enabledPluginNames.add(name);
                PluginSystem.saveEnabledPlugins();
                source.sendMessage("The plugin has been successfully enabled. "
                    + "Restart the bot to activate it.");
            }
            else if (command.equals("disable"))
            {
                String name = afterCommand;
                if (name.equals(""))
                    throw new ResponseException("You need to specify the name "
                        + "of the plugin to disable, like \"plugin disable "
                        + "some-example-plugin\".");
                if (!PluginSystem.enabledPluginNames.contains(name))
                    throw new ResponseException("That plugin is not currently "
                        + "enabled. If it's still active, you should try "
                        + "restarting the bot to deactivate it.");
                PluginSystem.enabledPluginNames.remove(name);
                PluginSystem.saveEnabledPlugins();
                source.sendMessage("The plugin has been successfully disabled. "
                    + "Restart the bot to deactivate it.");
            }
            else if (command.equals("info"))
            {
                String name = afterCommand;
                if (name.equals(""))
                    throw new ResponseException("You need to specify the name "
                        + "of the plugin whose information you want to read, "
                        + "like \"plugin info some-example-plugin\".");
                Plugin plugin = PluginSystem.knownPluginMap.get(name);
                if (plugin == null)
                    throw new ResponseException("That plugin is not an "
                        + "available plugin. The plugin might not be "
                        + "installed, or its language support plugin might "
                        + "not currently be active.");
                String response =
                        "Plugin " + plugin.info.name + "; language: " + plugin.language
                            + "; folder: " + plugin.folder.getPath() + "; ";
                if (plugin.info.dependencies.length == 0)
                    response += "No dependencies";
                else
                    response +=
                            "Dependencies: "
                                + StringUtils.delimited(plugin.info.dependencies, " ");
                
                response +=
                        "; Enabled: " + PluginSystem.enabledPluginNames.contains(name)
                            + "; Active: " + PluginSystem.loadedPluginNames.contains(name)
                            + "; Description: " + plugin.info.description;
                if (response.length() > (source.getProtocolDelimitedLength() * 2))
                    response = PastebinUtils.pastebinNotice(response, null);
                source.sendSpaced(response);
            }
            else if (command.equals("error"))
            {
                if (afterCommand.equals(""))
                    throw new ResponseException("You need to specify the "
                        + "name of the plugin whose error you want to see.");
                String traceback = PluginSystem.failedPluginTracebacks.get(afterCommand);
                if (traceback == null)
                    throw new ResponseException("The specified plugin did "
                        + "not encounter an error while loading.");
                source
                        .sendSpaced("Details of the error encountering "
                            + "while the plugin loaded: "
                            + PastebinUtils.pastebinNotice(traceback));
            }
            else
            {
                throw new ResponseException("Invalid plugin command. Try 'plugin "
                    + "<list|enable|disable|info|delete|undelete|error>'");
            }
        }
    }
    
    @Override
    public boolean relevant(String server, String channel, boolean pm,
            UserMessenger sender, Messenger source, String arguments)
    {
        return true;
    }
}
