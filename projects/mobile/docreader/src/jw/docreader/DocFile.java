package jw.docreader;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.lang.Thread.UncaughtExceptionHandler;

public class DocFile
{
    private RandomAccessFile file;
    
    public static final int VERSION = 1;
    
    private UncaughtExceptionHandler handler;
    
    public DocFile(File f, UncaughtExceptionHandler handler) throws IOException
    {
        file = new RandomAccessFile(f, "rw");
        file.seek(0);
        int fileVersion = file.readInt();
        if (fileVersion != VERSION)
            throw new IOException("Mismatched version; this interpreter reads version "
                    + VERSION + " but the file is of the format specified by version "
                    + fileVersion);
    }
    
    public int readDocCount() throws IOException
    {
        file.seek(4);
        return file.readInt();
    }
    
    public DocInfo readDocInfo(int index)
    {
        return null;
    }
}
