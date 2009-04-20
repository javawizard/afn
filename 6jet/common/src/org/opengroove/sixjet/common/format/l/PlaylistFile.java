package org.opengroove.sixjet.common.format.l;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Scanner;

public class PlaylistFile implements Serializable
{
    /**
     * 
     */
    private static final long serialVersionUID = -9158824689798013180L;
    
    public static class PlaylistItem implements Serializable
    {
        
        /**
         * 
         */
        private static final long serialVersionUID = 6431796801693869648L;
        private String id;
        private String type;
        private int duration;
        private String music;
        
        public String getId()
        {
            return id;
        }
        
        public String getType()
        {
            return type;
        }
        
        public int getDuration()
        {
            return duration;
        }
        
        public String getMusic()
        {
            return music;
        }
        
        public void setId(String id)
        {
            this.id = id;
        }
        
        public void setType(String type)
        {
            this.type = type;
        }
        
        public void setDuration(int duration)
        {
            this.duration = duration;
        }
        
        public void setMusic(String music)
        {
            this.music = music;
        }
    }
    
    private ArrayList<PlaylistItem> items = new ArrayList<PlaylistItem>();
    
    public PlaylistFile(InputStream in)
    {
        Scanner scanner = new Scanner(in);
        scanner.useDelimiter("\n");
        while (scanner.hasNext())
        {
            String token = scanner.next();
            token = token.trim();
            if (token.equals(""))
                continue;
            Scanner line = new Scanner(token);
            line.useDelimiter("\\|");
            PlaylistItem item = new PlaylistItem();
            String lineId = line.next();
            item.setId(lineId);
            String type = line.next();
            item.setType(type);
            if (type.equals("music"))
            {
                item.setMusic(line.next());
            }
            else if (type.equals("delay"))
            {
                item.setDuration(line.nextInt());
            }
            else
                throw new RuntimeException("Line type " + type
                    + " is not music or delay");
            items.add(item);
        }
        try
        {
            in.close();
        }
        catch (IOException e)
        {
            throw new RuntimeException(e);
        }
    }
}
