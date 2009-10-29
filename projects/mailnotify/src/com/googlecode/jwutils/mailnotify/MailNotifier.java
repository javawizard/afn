package com.googlecode.jwutils.mailnotify;

import java.util.Properties;

public class MailNotifier
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        String host ="";
        String username = "";
        String password = "";

        // Create empty properties
        Properties props = new Properties();

        // Get session
        Session session = Session.getDefaultInstance(props, null);

        // Get the store
        Store store = session.getStore("pop3");
        store.connect(host, username, password);

        // Get folder
        Folder folder = store.getFolder("INBOX");
        folder.open(Folder.READ_ONLY);

        // Get directory
        Message message[] = folder.getMessages();

        for (int i=0, n=message.length; i<n; i++) {
           System.out.println(i + ": " + message[i].getFrom()[0] 
             + "\t" + message[i].getSubject());
        }

        // Close connection 
        folder.close(false);
        store.close();
    }
    
}
