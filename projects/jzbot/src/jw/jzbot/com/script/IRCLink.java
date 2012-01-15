package jw.jzbot.com.script;

import jw.jzbot.FunctionNotifier;

import org.jibble.pircbot.DccChat;
import org.jibble.pircbot.DccFileTransfer;
import org.jibble.pircbot.PircBot;
import org.jibble.pircbot.User;
import org.mozilla.javascript.Function;

public class IRCLink extends PircBot implements ProtocolLink
{
    
    @Override
    protected void onAction(String sender, String login, String hostname,
            String target, String action)
    {
        
        notifierOnAction.call(sender, login, hostname, target, action);
    }
    
    private FunctionNotifier notifierOnAction = new FunctionNotifier();
    
    public void addActionListener(Function listener)
    {
        notifierOnAction.add(listener);
    }
    
    public void removeActionListener(Function listener)
    {
        notifierOnAction.remove(listener);
    }
    
    @Override
    protected void onChannelInfo(String channel, int userCount, String topic)
    {
        
        notifierOnChannelInfo.call(channel, userCount, topic);
    }
    
    private FunctionNotifier notifierOnChannelInfo = new FunctionNotifier();
    
    public void addChannelInfoListener(Function listener)
    {
        notifierOnChannelInfo.add(listener);
    }
    
    public void removeChannelInfoListener(Function listener)
    {
        notifierOnChannelInfo.remove(listener);
    }
    
    @Override
    protected void onConnect()
    {
        
        notifierOnConnect.call();
    }
    
    private FunctionNotifier notifierOnConnect = new FunctionNotifier();
    
    public void addConnectListener(Function listener)
    {
        notifierOnConnect.add(listener);
    }
    
    public void removeConnectListener(Function listener)
    {
        notifierOnConnect.remove(listener);
    }
    
    @Override
    protected void onDeop(String channel, String sourceNick,
            String sourceLogin, String sourceHostname, String recipient)
    {
        
        notifierOnDeop.call(channel, sourceNick, sourceLogin, sourceHostname,
                recipient);
    }
    
    private FunctionNotifier notifierOnDeop = new FunctionNotifier();
    
    public void addDeopListener(Function listener)
    {
        notifierOnDeop.add(listener);
    }
    
    public void removeDeopListener(Function listener)
    {
        notifierOnDeop.remove(listener);
    }
    
    @Override
    protected void onDeVoice(String channel, String sourceNick,
            String sourceLogin, String sourceHostname, String recipient)
    {
        
        notifierOnDeVoice.call(channel, sourceNick, sourceLogin,
                sourceHostname, recipient);
    }
    
    private FunctionNotifier notifierOnDeVoice = new FunctionNotifier();
    
    public void addDeVoiceListener(Function listener)
    {
        notifierOnDeVoice.add(listener);
    }
    
    public void removeDeVoiceListener(Function listener)
    {
        notifierOnDeVoice.remove(listener);
    }
    
    @Override
    protected void onDisconnect()
    {
        
        notifierOnDisconnect.call();
    }
    
    private FunctionNotifier notifierOnDisconnect = new FunctionNotifier();
    
    public void addDisconnectListener(Function listener)
    {
        notifierOnDisconnect.add(listener);
    }
    
    public void removeDisconnectListener(Function listener)
    {
        notifierOnDisconnect.remove(listener);
    }
    
    @Override
    protected void onInvite(String targetNick, String sourceNick,
            String sourceLogin, String sourceHostname, String channel)
    {
        
        notifierOnInvite.call(targetNick, sourceNick, sourceLogin,
                sourceHostname, channel);
    }
    
    private FunctionNotifier notifierOnInvite = new FunctionNotifier();
    
    public void addInviteListener(Function listener)
    {
        notifierOnInvite.add(listener);
    }
    
    public void removeInviteListener(Function listener)
    {
        notifierOnInvite.remove(listener);
    }
    
    @Override
    protected void onJoin(String channel, String sender, String login,
            String hostname)
    {
        
        notifierOnJoin.call(channel, sender, login, hostname);
    }
    
    private FunctionNotifier notifierOnJoin = new FunctionNotifier();
    
    public void addJoinListener(Function listener)
    {
        notifierOnJoin.add(listener);
    }
    
    public void removeJoinListener(Function listener)
    {
        notifierOnJoin.remove(listener);
    }
    
    @Override
    protected void onKick(String channel, String kickerNick,
            String kickerLogin, String kickerHostname, String recipientNick,
            String reason)
    {
        
        notifierOnKick.call(channel, kickerNick, kickerLogin, kickerHostname,
                recipientNick, reason);
    }
    
    private FunctionNotifier notifierOnKick = new FunctionNotifier();
    
    public void addKickListener(Function listener)
    {
        notifierOnKick.add(listener);
    }
    
    public void removeKickListener(Function listener)
    {
        notifierOnKick.remove(listener);
    }
    
    @Override
    protected void onMessage(String channel, String sender, String login,
            String hostname, String message)
    {
        
        notifierOnMessage.call(channel, sender, login, hostname, message);
    }
    
    private FunctionNotifier notifierOnMessage = new FunctionNotifier();
    
    public void addMessageListener(Function listener)
    {
        notifierOnMessage.add(listener);
    }
    
    public void removeMessageListener(Function listener)
    {
        notifierOnMessage.remove(listener);
    }
    
    @Override
    protected void onMode(String channel, String sourceNick,
            String sourceLogin, String sourceHostname, String mode)
    {
        
        notifierOnMode.call(channel, sourceNick, sourceLogin, sourceHostname,
                mode);
    }
    
    private FunctionNotifier notifierOnMode = new FunctionNotifier();
    
    public void addModeListener(Function listener)
    {
        notifierOnMode.add(listener);
    }
    
    public void removeModeListener(Function listener)
    {
        notifierOnMode.remove(listener);
    }
    
    @Override
    protected void onNickChange(String oldNick, String login, String hostname,
            String newNick)
    {
        
        notifierOnNickChange.call(oldNick, login, hostname, newNick);
    }
    
    private FunctionNotifier notifierOnNickChange = new FunctionNotifier();
    
    public void addNickChangeListener(Function listener)
    {
        notifierOnNickChange.add(listener);
    }
    
    public void removeNickChangeListener(Function listener)
    {
        notifierOnNickChange.remove(listener);
    }
    
    @Override
    protected void onNotice(String sourceNick, String sourceLogin,
            String sourceHostname, String target, String notice)
    {
        
        notifierOnNotice.call(sourceNick, sourceLogin, sourceHostname, target,
                notice);
    }
    
    private FunctionNotifier notifierOnNotice = new FunctionNotifier();
    
    public void addNoticeListener(Function listener)
    {
        notifierOnNotice.add(listener);
    }
    
    public void removeNoticeListener(Function listener)
    {
        notifierOnNotice.remove(listener);
    }
    
    @Override
    protected void onOp(String channel, String sourceNick, String sourceLogin,
            String sourceHostname, String recipient)
    {
        
        notifierOnOp.call(channel, sourceNick, sourceLogin, sourceHostname,
                recipient);
    }
    
    private FunctionNotifier notifierOnOp = new FunctionNotifier();
    
    public void addOpListener(Function listener)
    {
        notifierOnOp.add(listener);
    }
    
    public void removeOpListener(Function listener)
    {
        notifierOnOp.remove(listener);
    }
    
    @Override
    protected void onPart(String channel, String sender, String login,
            String hostname)
    {
        
        notifierOnPart.call(channel, sender, login, hostname);
    }
    
    private FunctionNotifier notifierOnPart = new FunctionNotifier();
    
    public void addPartListener(Function listener)
    {
        notifierOnPart.add(listener);
    }
    
    public void removePartListener(Function listener)
    {
        notifierOnPart.remove(listener);
    }
    
    @Override
    protected void onPing(String sourceNick, String sourceLogin,
            String sourceHostname, String target, String pingValue)
    {
        
        super
                .onPing(sourceNick, sourceLogin, sourceHostname, target,
                        pingValue);
    }
    
    @Override
    protected void onPrivateMessage(String sender, String login,
            String hostname, String message)
    {
        
        notifierOnPrivateMessage.call(sender, login, hostname, message);
    }
    
    private FunctionNotifier notifierOnPrivateMessage = new FunctionNotifier();
    
    public void addPrivateMessageListener(Function listener)
    {
        notifierOnPrivateMessage.add(listener);
    }
    
    public void removePrivateMessageListener(Function listener)
    {
        notifierOnPrivateMessage.remove(listener);
    }
    
    @Override
    protected void onQuit(String sourceNick, String sourceLogin,
            String sourceHostname, String reason)
    {
        
        notifierOnQuit.call(sourceNick, sourceLogin, sourceHostname, reason);
    }
    
    private FunctionNotifier notifierOnQuit = new FunctionNotifier();
    
    public void addQuitListener(Function listener)
    {
        notifierOnQuit.add(listener);
    }
    
    public void removeQuitListener(Function listener)
    {
        notifierOnQuit.remove(listener);
    }
    
    @Override
    protected void onRemoveChannelBan(String channel, String sourceNick,
            String sourceLogin, String sourceHostname, String hostmask)
    {
        
        notifierOnRemoveChannelBan.call(channel, sourceNick, sourceLogin,
                sourceHostname, hostmask);
    }
    
    private FunctionNotifier notifierOnRemoveChannelBan = new FunctionNotifier();
    
    public void addRemoveChannelBanListener(Function listener)
    {
        notifierOnRemoveChannelBan.add(listener);
    }
    
    public void removeRemoveChannelBanListener(Function listener)
    {
        notifierOnRemoveChannelBan.remove(listener);
    }
    
    @Override
    protected void onRemoveChannelKey(String channel, String sourceNick,
            String sourceLogin, String sourceHostname, String key)
    {
        
        notifierOnRemoveChannelKey.call(channel, sourceNick, sourceLogin,
                sourceHostname, key);
    }
    
    private FunctionNotifier notifierOnRemoveChannelKey = new FunctionNotifier();
    
    public void addRemoveChannelKeyListener(Function listener)
    {
        notifierOnRemoveChannelKey.add(listener);
    }
    
    public void removeRemoveChannelKeyListener(Function listener)
    {
        notifierOnRemoveChannelKey.remove(listener);
    }
    
    @Override
    protected void onRemoveChannelLimit(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveChannelLimit.call(channel, sourceNick, sourceLogin,
                sourceHostname);
    }
    
    private FunctionNotifier notifierOnRemoveChannelLimit = new FunctionNotifier();
    
    public void addRemoveChannelLimitListener(Function listener)
    {
        notifierOnRemoveChannelLimit.add(listener);
    }
    
    public void removeRemoveChannelLimitListener(Function listener)
    {
        notifierOnRemoveChannelLimit.remove(listener);
    }
    
    @Override
    protected void onRemoveInviteOnly(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveInviteOnly.call(channel, sourceNick, sourceLogin,
                sourceHostname);
    }
    
    private FunctionNotifier notifierOnRemoveInviteOnly = new FunctionNotifier();
    
    public void addRemoveInviteOnlyListener(Function listener)
    {
        notifierOnRemoveInviteOnly.add(listener);
    }
    
    public void removeRemoveInviteOnlyListener(Function listener)
    {
        notifierOnRemoveInviteOnly.remove(listener);
    }
    
    @Override
    protected void onRemoveModerated(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveModerated.call(channel, sourceNick, sourceLogin,
                sourceHostname);
    }
    
    private FunctionNotifier notifierOnRemoveModerated = new FunctionNotifier();
    
    public void addRemoveModeratedListener(Function listener)
    {
        notifierOnRemoveModerated.add(listener);
    }
    
    public void removeRemoveModeratedListener(Function listener)
    {
        notifierOnRemoveModerated.remove(listener);
    }
    
    @Override
    protected void onRemoveNoExternalMessages(String channel,
            String sourceNick, String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveNoExternalMessages.call(channel, sourceNick,
                sourceLogin, sourceHostname);
    }
    
    private FunctionNotifier notifierOnRemoveNoExternalMessages = new FunctionNotifier();
    
    public void addRemoveNoExternalMessagesListener(Function listener)
    {
        notifierOnRemoveNoExternalMessages.add(listener);
    }
    
    public void removeRemoveNoExternalMessagesListener(Function listener)
    {
        notifierOnRemoveNoExternalMessages.remove(listener);
    }
    
    @Override
    protected void onRemovePrivate(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemovePrivate.call(channel, sourceNick, sourceLogin,
                sourceHostname);
    }
    
    private FunctionNotifier notifierOnRemovePrivate = new FunctionNotifier();
    
    public void addRemovePrivateListener(Function listener)
    {
        notifierOnRemovePrivate.add(listener);
    }
    
    public void removeRemovePrivateListener(Function listener)
    {
        notifierOnRemovePrivate.remove(listener);
    }
    
    @Override
    protected void onRemoveSecret(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveSecret.call(channel, sourceNick, sourceLogin,
                sourceHostname);
    }
    
    private FunctionNotifier notifierOnRemoveSecret = new FunctionNotifier();
    
    public void addRemoveSecretListener(Function listener)
    {
        notifierOnRemoveSecret.add(listener);
    }
    
    public void removeRemoveSecretListener(Function listener)
    {
        notifierOnRemoveSecret.remove(listener);
    }
    
    @Override
    protected void onRemoveTopicProtection(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveSecret.call(channel, sourceNick, sourceLogin,
                sourceHostname);
        
    }
    
    private FunctionNotifier notifierOnRemoveTopicProtection = new FunctionNotifier();
    
    public void addRemoveTopicProtectionListener(Function listener)
    {
        notifierOnRemoveTopicProtection.add(listener);
    }
    
    public void removeRemoveTopicProtectionListener(Function listener)
    {
        notifierOnRemoveTopicProtection.remove(listener);
    }
    
    @Override
    protected void onServerResponse(int code, String response)
    {
        
        notifierOnServerResponse.call(code, response);
    }
    
    private FunctionNotifier notifierOnServerResponse = new FunctionNotifier();
    
    public void addServerResponseListener(Function listener)
    {
        notifierOnServerResponse.add(listener);
    }
    
    public void removeServerResponseListener(Function listener)
    {
        notifierOnServerResponse.remove(listener);
    }
    
    @Override
    protected void onSetChannelBan(String channel, String sourceNick,
            String sourceLogin, String sourceHostname, String hostmask)
    {
        
        notifierOnSetChannelBan.call(channel, sourceNick, sourceLogin,
                sourceHostname, hostmask);
    }
    
    private FunctionNotifier notifierOnSetChannelBan = new FunctionNotifier();
    
    public void addSetChannelBanListener(Function listener)
    {
        notifierOnSetChannelBan.add(listener);
    }
    
    public void removeSetChannelBanListener(Function listener)
    {
        notifierOnSetChannelBan.remove(listener);
    }
    
    @Override
    protected void onSetChannelKey(String channel, String sourceNick,
            String sourceLogin, String sourceHostname, String key)
    {
        
        notifierOnSetChannelKey.call(channel, sourceNick, sourceLogin,
                sourceHostname, key);
    }
    
    private FunctionNotifier notifierOnSetChannelKey = new FunctionNotifier();
    
    public void addSetChannelKeyListener(Function listener)
    {
        notifierOnSetChannelKey.add(listener);
    }
    
    public void removeSetChannelKeyListener(Function listener)
    {
        notifierOnSetChannelKey.remove(listener);
    }
    
    @Override
    protected void onSetChannelLimit(String channel, String sourceNick,
            String sourceLogin, String sourceHostname, int limit)
    {
        
        notifierOnSetChannelLimit.call(channel, sourceNick, sourceLogin,
                sourceHostname, limit);
    }
    
    private FunctionNotifier notifierOnSetChannelLimit = new FunctionNotifier();
    
    public void addSetChannelLimitListener(Function listener)
    {
        notifierOnSetChannelLimit.add(listener);
    }
    
    public void removeSetChannelLimitListener(Function listener)
    {
        notifierOnSetChannelLimit.remove(listener);
    }
    
    @Override
    protected void onSetInviteOnly(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnSetInviteOnly.call(channel, sourceNick, sourceLogin,
                sourceHostname);
    }
    
    private FunctionNotifier notifierOnSetInviteOnly = new FunctionNotifier();
    
    public void addSetInviteOnlyListener(Function listener)
    {
        notifierOnSetInviteOnly.add(listener);
    }
    
    public void removeSetInviteOnlyListener(Function listener)
    {
        notifierOnSetInviteOnly.remove(listener);
    }
    
    @Override
    protected void onSetModerated(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnSetModerated.call(channel, sourceNick, sourceLogin,
                sourceHostname);
    }
    
    private FunctionNotifier notifierOnSetModerated = new FunctionNotifier();
    
    public void addSetModeratedListener(Function listener)
    {
        notifierOnSetModerated.add(listener);
    }
    
    public void removeSetModeratedListener(Function listener)
    {
        notifierOnSetModerated.remove(listener);
    }
    
    @Override
    protected void onSetNoExternalMessages(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveSecret.call(channel, sourceNick, sourceLogin,
                sourceHostname);
        
    }
    
    private FunctionNotifier notifierOnSetNoExternalMessages = new FunctionNotifier();
    
    public void addSetNoExternalMessagesListener(Function listener)
    {
        notifierOnSetNoExternalMessages.add(listener);
    }
    
    public void removeSetNoExternalMessagesListener(Function listener)
    {
        notifierOnSetNoExternalMessages.remove(listener);
    }
    
    @Override
    protected void onSetPrivate(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveSecret.call(channel, sourceNick, sourceLogin,
                sourceHostname);
        
    }
    
    private FunctionNotifier notifierOnSetPrivate = new FunctionNotifier();
    
    public void addSetPrivateListener(Function listener)
    {
        notifierOnSetPrivate.add(listener);
    }
    
    public void removeSetPrivateListener(Function listener)
    {
        notifierOnSetPrivate.remove(listener);
    }
    
    @Override
    protected void onSetSecret(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveSecret.call(channel, sourceNick, sourceLogin,
                sourceHostname);
        
    }
    
    private FunctionNotifier notifierOnSetSecret = new FunctionNotifier();
    
    public void addSetSecretListener(Function listener)
    {
        notifierOnSetSecret.add(listener);
    }
    
    public void removeSetSecretListener(Function listener)
    {
        notifierOnSetSecret.remove(listener);
    }
    
    @Override
    protected void onSetTopicProtection(String channel, String sourceNick,
            String sourceLogin, String sourceHostname)
    {
        
        notifierOnRemoveSecret.call(channel, sourceNick, sourceLogin,
                sourceHostname);
        
    }
    
    private FunctionNotifier notifierOnSetTopicProtection = new FunctionNotifier();
    
    public void addSetTopicProtectionListener(Function listener)
    {
        notifierOnSetTopicProtection.add(listener);
    }
    
    public void removeSetTopicProtectionListener(Function listener)
    {
        notifierOnSetTopicProtection.remove(listener);
    }
    
    @Override
    protected void onTopic(String channel, String topic, String setBy,
            long date, boolean changed)
    {
        
        notifierOnTopic.call(channel, topic, setBy, date, changed);
    }
    
    private FunctionNotifier notifierOnTopic = new FunctionNotifier();
    
    public void addTopicListener(Function listener)
    {
        notifierOnTopic.add(listener);
    }
    
    public void removeTopicListener(Function listener)
    {
        notifierOnTopic.remove(listener);
    }
    
    @Override
    protected void onUnknown(String line)
    {
        
        notifierOnUnknown.call(line);
    }
    
    private FunctionNotifier notifierOnUnknown = new FunctionNotifier();
    
    public void addUnknownListener(Function listener)
    {
        notifierOnUnknown.add(listener);
    }
    
    public void removeUnknownListener(Function listener)
    {
        notifierOnUnknown.remove(listener);
    }
    
    @Override
    protected void onUserList(String channel, User[] users)
    {
        notifierOnUserList.call(channel, users);
    }
    
    private FunctionNotifier notifierOnUserList = new FunctionNotifier();
    
    public void addUserListListener(Function listener)
    {
        notifierOnUserList.add(listener);
    }
    
    public void removeUserListListener(Function listener)
    {
        notifierOnUserList.remove(listener);
    }
    
    @Override
    protected void onUserMode(String targetNick, String sourceNick,
            String sourceLogin, String sourceHostname, String mode)
    {
        notifierOnUserMode.call(targetNick, sourceNick, sourceLogin,
                sourceHostname, mode);
    }
    
    private FunctionNotifier notifierOnUserMode = new FunctionNotifier();
    
    public void addUserModeListener(Function listener)
    {
        notifierOnUserMode.add(listener);
    }
    
    public void removeUserModeListener(Function listener)
    {
        notifierOnUserMode.remove(listener);
    }
    
    @Override
    protected void onVoice(String channel, String sourceNick,
            String sourceLogin, String sourceHostname, String recipient)
    {
        notifierOnVoice.call(channel, sourceNick, sourceLogin, sourceHostname,
                recipient);
    }
    
    private FunctionNotifier notifierOnVoice = new FunctionNotifier();
    
    public void addVoiceListener(Function listener)
    {
        notifierOnVoice.add(listener);
    }
    
    public void removeVoiceListener(Function listener)
    {
        notifierOnVoice.remove(listener);
    }
    
    @Override
    public void shutdown()
    {
        notifierOnDisconnect.clear();
        disconnect();
    }
    
    public String toString()
    {
        return "IRCLink instance (" + super.toString() + ")";
    }
    
    public void setPublicName(String name)
    {
        super.setName(name);
    }
    
    public void setPublicLogin(String login)
    {
        super.setLogin(login);
    }
}
