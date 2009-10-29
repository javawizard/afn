package com.googlecode.jwutils.mailnotify;

import java.io.File;
import java.util.Properties;

import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.Flags.Flag;
import javax.mail.search.FlagTerm;
import javax.mail.search.SearchTerm;

import net.sf.opengroove.common.utils.StringUtils;

public class MailNotifierTest
{
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        long time = System.currentTimeMillis();
        String host = "imap.gmail.com";
        String username = StringUtils.readFile(new File("storage/username"));
        String password = StringUtils.readFile(new File("storage/password"));
        
        // Create empty properties
        Properties props = new Properties();
        final String SSL_FACTORY = "javax.net.ssl.SSLSocketFactory";
        // IMAP provider
        props.setProperty("mail.imap.socketFactory.class", SSL_FACTORY);
        // Get session
        Session session = Session.getDefaultInstance(props, null);
        
        // Get the store
        Store store = session.getStore("imap");
        System.out.println("connecting");
        store.connect(host, 993, username, password);
        System.out.println("getting folder");
        // Get folder
        Folder folder = store.getFolder("INBOX");
        folder.open(Folder.READ_ONLY);
        
        System.out.println("Checking unread");
        System.out.println("unread: " + folder.getUnreadMessageCount());
        // Get directory
        Message[] messages = folder.search(new FlagTerm(new Flags(Flag.SEEN), false));
        
        for (Message m : messages)
        {
            System.out.println("From " + m.getFrom()[0] + ": " + m.getSubject());
        }
        
        // Close connection
        folder.close(false);
        store.close();
        System.out.println("Took " + (System.currentTimeMillis() - time) + "ms");
    }
    
}
