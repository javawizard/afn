package com.googlecode.jwutils.mailnotify;

import java.io.File;
import java.util.Properties;

import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Store;

import net.sf.opengroove.common.utils.StringUtils;

public class MailNotifier
{
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        String host = "imap.gmail.com";
        String username = StringUtils.readFile(new File("storage/username"));
        String password = StringUtils.readFile(new File("storage/password"));
        
        // Create empty properties
        Properties props = new Properties();
        
        // Get session
        Session session = Session.getDefaultInstance(props, null);
        
        // Get the store
        Store store = session.getStore("imap");
        store.connect(host, 993, username, password);
        
        // Get folder
        Folder folder = store.getFolder("INBOX");
        folder.open(Folder.READ_ONLY);
        
        // Get directory
        Message message[] = folder.getMessages();
        
        for (int i = 0, n = message.length; i < n; i++)
        {
            System.out.println(i + ": " + message[i].getFrom()[0] + "\t"
                    + message[i].getSubject());
        }
        
        // Close connection
        folder.close(false);
        store.close();
    }
    
}
