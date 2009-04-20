package org.opengroove.sixjet.common.format.l;

import java.io.InputStream;
import java.io.Serializable;

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
    }
    
    public PlaylistFile(InputStream in)
    {
        
    }
}
